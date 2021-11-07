{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}

module Server.Start
    ( startServer
    , ServerConfig(..)
    ) where

import qualified App.Logger as L
import qualified App.ServerHandler as SH
import qualified App.ServerHandler.Instance as SHI
import qualified Control.Monad.Catch as CMC
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.Proxy
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as PS
import qualified Exceptions as Ex
import qualified Network.Wai.Handler.Warp as NWW
import qualified Servant as S
import Servant ((:>))
import Server (executeWithErrorHandlers)
import Server.Result
import Types

type CacheAPI
     = "weather" :> S.QueryParam "time" Int :> S.QueryParam "city_id" Int :> S.QueryParam "city_name" T.Text :> S.QueryParam "lat" Double :> S.QueryParam "lon" Double :> S.Get '[ S.JSON] Result

api :: Proxy CacheAPI
api = Proxy

server ::
       SHI.Config
    -> L.LoggerHandler IO
    -> IORef SHI.Resources
    -> S.Server CacheAPI
server config logger resourcesRef =
    \mTime mCityID mCityName mLat mLon ->
        liftIO $ do
            resources <- readIORef resourcesRef
            let handle = SHI.resourcesToHandle config resources logger
            (res, resources') <-
                Ex.withExceptionHandlers
                    (resourcesErrorHandlers logger resources) $
                (, resources) <$>
                serverIO handle (Seconds <$> mTime) mCityID mCityName mLat mLon
            writeIORef resourcesRef resources'
            pure res

defaultHandler ::
       L.LoggerHandler IO
    -> SHI.Resources
    -> CMC.SomeException
    -> IO (Result, SHI.Resources)
defaultHandler logger resources e = do
    L.logError logger "unexpected error occured"
    L.logError logger $ T.pack $ CMC.displayException e
    pure (internalError, resources)

resourcesErrorHandlers ::
       L.LoggerHandler IO
    -> SHI.Resources
    -> [CMC.Handler IO (Result, SHI.Resources)]
resourcesErrorHandlers logger resources =
    [CMC.Handler (defaultHandler logger resources)]

serverIO ::
       SH.ServerHandler IO
    -> Maybe Seconds
    -> Maybe Int
    -> Maybe T.Text
    -> Maybe Double
    -> Maybe Double
    -> IO Result
serverIO h mTime mCityID mCityName mLat mLon =
    liftIO $ executeWithErrorHandlers h mTime mCityID mCityName mLat mLon

data ServerConfig =
    ServerConfig
        { sconfPGConnection :: PS.Connection
        , sconfLogger :: L.LoggerHandler IO
        , sconfApiKey :: T.Text
        , sconfDelta :: Delta
        , sconfPort :: Int
        }

startServer :: ServerConfig -> IO ()
startServer serverConfig = do
    let con = sconfPGConnection serverConfig
        delta = sconfDelta serverConfig
        environment = SH.Environment {SH.envDelta = delta}
        config =
            SHI.Config
                { SHI.configEnv = environment
                , SHI.configApiKey = sconfApiKey serverConfig
                }
        resources = SHI.Resources con
    resourcesRef <- newIORef resources
    let logger = sconfLogger serverConfig
        server' = server config logger resourcesRef
        app = S.serve api server'
    NWW.run (sconfPort serverConfig) app
