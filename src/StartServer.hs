{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}

module StartServer where


import Data.IORef
import qualified App.Logger as L
import qualified App.ServerHandler as SH
import qualified App.ServerHandler.Instance as SHI
import Types
import qualified Servant as S
import Servant ((:>))
import qualified Data.Text as T
import Data.Proxy
import qualified Database.PostgreSQL.Simple as PS
import Server (executeWithErrorHandlers)
import Control.Monad.IO.Class (liftIO)
import qualified Network.Wai.Handler.Warp as NWW
import qualified Exceptions as Ex
import qualified Control.Monad.Catch as CMC
import Server.Result

type CacheAPI
   = "weather" :> S.QueryParam "time" Integer
        :> S.QueryParam "city_id" Int
        :> S.QueryParam "city_name" T.Text
        :> S.QueryParam "lat" Double :> S.QueryParam "lon" Double
        :> S.Get '[ S.JSON] Result

api :: Proxy CacheAPI
api = Proxy

server :: SHI.Config -> L.LoggerHandler IO -> IORef SHI.Resources -> S.Server CacheAPI
server config logger resourcesRef = \mTime mCityID mCityName mLat mLon -> liftIO $ do
    resources <- readIORef resourcesRef
    let handle = SHI.resourcesToHandle config resources logger
    (res, resources') <- Ex.withExceptionHandlers (resourcesErrorHandlers logger resources) $
        (, resources) <$> serverIO handle mTime mCityID mCityName mLat mLon
    writeIORef resourcesRef resources'
    pure res

defaultHandler :: L.LoggerHandler IO -> SHI.Resources -> CMC.SomeException -> IO (Result, SHI.Resources)
defaultHandler logger resources e = do
    L.logError logger "unexpected error occured"
    L.logError logger $ T.pack $ CMC.displayException e
    pure $ (internalError, resources)
 
resourcesErrorHandlers :: L.LoggerHandler IO -> SHI.Resources -> [CMC.Handler IO (Result, SHI.Resources)]
resourcesErrorHandlers logger resources =
    [ CMC.Handler (defaultHandler logger resources)
    ]  


serverIO :: SH.ServerHandler IO -> Maybe Integer -> Maybe Int -> Maybe T.Text -> Maybe Double -> Maybe Double -> IO Result
serverIO h mTime mCityID mCityName mLat mLon =
    liftIO $ executeWithErrorHandlers h mTime mCityID mCityName mLat mLon

data ServerConfig = ServerConfig {
    sconfPGConnection :: PS.Connection
    , sconfLogger :: L.LoggerHandler IO

    , sconfApiKey :: T.Text
    , sconfDelta :: Delta
    , sconfPort :: Int
    }


startServer :: ServerConfig -> IO ()
startServer serverConfig = do
    let con = sconfPGConnection serverConfig
        delta = sconfDelta serverConfig
        environment = SH.Environment
            { SH.envDelta = delta }
        config = SHI.Config {
            SHI.configEnv = environment
            , SHI.configApiKey = sconfApiKey serverConfig
            }
        resources = SHI.Resources con
    resourcesRef <- newIORef resources
    let 
        logger = sconfLogger serverConfig
        server' = server config logger resourcesRef
        app = S.serve api server'
    NWW.run (sconfPort serverConfig) $ app
 
