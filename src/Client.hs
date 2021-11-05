{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module Client where

import Data.Proxy
import qualified Data.Text as T
import qualified GenericPretty as GP
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.API
import qualified Servant.Client as SC
import Servant.Types.SourceT (foreach)
import Types
import Control.Monad.Catch as CMC

import qualified Servant.Client.Streaming as S

import qualified Database.PostgreSQL.Simple as PS
import qualified App.Logger as L
import Utils as U

{-
type API = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
      :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
      :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email
-}
type OpenWeatherAPI
   = QueryParam "appid" T.Text :> QueryParam "id" Int :> Get '[ JSON] APIResponse

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

rzhevId :: Maybe Int
rzhevId = Just 499717

toIOThrow :: SC.ClientM a -> IO a
toIOThrow action = do
    eithRes <- toIOEither action
    either CMC.throwM pure eithRes

toIOEither :: SC.ClientM a -> IO (Either SC.ClientError a)
toIOEither action = do
  manager <- newManager defaultManagerSettings
  SC.runClientM action $ SC.mkClientEnv manager openWeatherBaseUrl

weatherByLocationIdIOEither ::
     Maybe T.Text -> Maybe Int -> IO (Either SC.ClientError APIResponse)
weatherByLocationIdIOEither key locationId = toIOEither $ weatherByLocationId key locationId

weatherByLocationIdIOThrow ::
     Maybe T.Text -> Maybe Int -> IO APIResponse
weatherByLocationIdIOThrow key locationId = toIOThrow $ weatherByLocationId key locationId
    

debug :: (GP.PrettyShow a, Show e) => IO (Either e a) -> IO ()
debug action = do
  eithRes <- action
  U.withEither
    eithRes
    (L.logDebug L.stdHandler . U.showText)
    (L.logDebug L.stdHandler . GP.textPretty)

unEither = either undefined id

connectString =
  "dbname=" <>
  databaseName <> " user=" <> userName <> " password='" <> password <> "'"
  where
    databaseName = "weatherdb"
    userName = "weather_owner"
    password = "0000"

connectMyDB :: IO PS.Connection
connectMyDB = PS.connectPostgreSQL connectString

insertQuery :: PS.Query
insertQuery =
  " INSERT INTO weather.cache VALUES\
    \(?, ?, \
    \ ? ,\
    \ ?, ?, ?, ?, \
    \ ?, ?, ?, ?, \
    \ ?, ?, ?, ?, ?, ?, \
    \ ?, ?, ?, \
    \ ?,   ?, ?,   ?, ?, \
    \ ?, ?, ?, ?, ?, ? )"

simpleTest :: IO ()
simpleTest = do
  let logger = L.stdHandler
  con <- connectMyDB
  L.logDebug logger "clearing table for test"
  _ <- PS.execute_ con "truncate table weather.cache"
  currentWeather <- weatherByLocationIdIOThrow apiKey rzhevId
  L.logDebug logger $ GP.textPretty currentWeather
  insertRes <- PS.execute con insertQuery currentWeather
  L.logDebug logger $ "inserted? " <> U.showText insertRes <> " rows"
  L.logDebug logger "trying to fetch this row again"
  fetched <- head <$> PS.query_ con "select * from weather.cache"
  L.logDebug logger "fetched row is"
  L.logDebug logger $ GP.textPretty fetched
  L.logDebug logger "are they equal?"
  L.logDebug logger $ U.showText $ fetched == currentWeather
  PS.close con
