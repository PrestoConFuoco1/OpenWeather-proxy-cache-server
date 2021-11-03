{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module Client where

import qualified Data.Text as T
import qualified GenericPretty as GP
import Types
import Data.Proxy
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import qualified Servant.Client as SC
import Servant.Types.SourceT (foreach)

import qualified Servant.Client.Streaming as S

import Utils as U
import qualified Logger as L

{-
type API = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
      :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
      :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email
-}

type OpenWeatherAPI = QueryParam "appid" T.Text :> QueryParam "id" Integer :> Get '[JSON] APIResponse

api :: Proxy OpenWeatherAPI
api = Proxy

--weatherByLocationID :: Maybe T.Text -> Maybe Integer -> ClientM Value
weatherByLocationId = SC.client api

baseScheme = SC.Http
baseUrlHost = "api.openweathermap.org"
baseUrlPort = 80
baseUrlPath = "data/2.5/weather"
openWeatherBaseUrl = S.BaseUrl baseScheme baseUrlHost baseUrlPort baseUrlPath

apiKey :: Maybe T.Text
apiKey = Just "08a9fc08a909b0f3de55b60ce736fbfa"

rzhevId :: Maybe Integer
rzhevId = Just 499717

toIO :: SC.ClientM a -> IO (Either SC.ClientError a)
toIO action = do
    manager <- newManager defaultManagerSettings
    SC.runClientM action $ SC.mkClientEnv manager openWeatherBaseUrl

weatherByLocationIdIO :: Maybe T.Text -> Maybe Integer -> IO (Either SC.ClientError APIResponse)
weatherByLocationIdIO key locationId = toIO $ weatherByLocationId key locationId

debug :: (GP.PrettyShow a, Show e) => IO (Either e a) -> IO ()
debug action = do
    eithRes <- action
    U.withEither eithRes
        (L.logDebug L.stdHandler . U.showText)
        (L.logDebug L.stdHandler . GP.textPretty)
    


