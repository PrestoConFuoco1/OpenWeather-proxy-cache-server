module Lib
  ( someFunc
  ) where

import StartServer (startServer, ServerConfig(..))
import StartCacheFiller (startCacheFiller, FillerConfig(..))
import qualified App.Logger as L
import qualified Database.PostgreSQL.Simple as PS
import Private (privateApiKey)
import qualified Config as C
import qualified GenericPretty as GP
import System.Environment (getEnv)
import qualified Data.Text as T
import RunOptions
import qualified Data.Text.Encoding as E
import qualified System.Exit as Q
import Control.Monad (when)

rzhev :: Int
rzhev = 499717

kostomuksha :: Int
kostomuksha = 543899

stPetersburg :: Int
stPetersburg = 4171563

moskvorechieSaburovo :: Int
moskvorechieSaburovo = 499622

cities :: [Int]
cities = [rzhev, kostomuksha, stPetersburg, moskvorechieSaburovo]

defaultFillerConfig :: PS.Connection -> L.LoggerHandler IO -> FillerConfig
defaultFillerConfig con logger = FillerConfig {
    fconfCities = cities
    , fconfApiKey = privateApiKey
    , fconfSleepTimeSeconds = 60*10
    , fconfPGConnection = con
    , fconfLogger = logger
    }

connectStringMyDB =
  "dbname=" <>
  databaseName <> " user=" <> userName <> " password='" <> password <> "'"
  where
    databaseName = "weatherdb"
    userName = "weather_owner"
    password = "0000"

connectMyDB :: IO PS.Connection
connectMyDB = PS.connectPostgreSQL connectStringMyDB



defaultServerPort = 8081
defaultTimeEpsSeconds = 1000

defaultServerConfig :: PS.Connection -> L.LoggerHandler IO -> ServerConfig
defaultServerConfig con logger = ServerConfig {
    sconfPGConnection = con
    , sconfLogger = logger
    , sconfApiKey = privateApiKey
    , sconfTimeEpsSeconds = defaultTimeEpsSeconds
    , sconfPort = defaultServerPort
    }

connectString :: C.Config -> T.Text
connectString conf =
  "dbname=" <>
  databaseName <> " user=" <> userName <> " password='" <> password <> "'"
  where
    databaseName = C.databaseName conf
    userName = C.databaseUser conf
    password = C.databasePassword conf

toFillersConfig :: T.Text -> C.Config -> PS.Connection -> L.LoggerHandler IO -> FillerConfig
toFillersConfig apiKey conf con logger = FillerConfig {
    fconfCities = C.fillerCities conf
    , fconfApiKey = apiKey
    , fconfSleepTimeSeconds = C.fillerSleepTimeSeconds conf
    , fconfPGConnection = con
    , fconfLogger = logger
    }

toServerConfig :: T.Text -> C.Config -> PS.Connection -> L.LoggerHandler IO -> ServerConfig
toServerConfig apiKey conf con logger = ServerConfig {
      sconfApiKey = apiKey
    , sconfTimeEpsSeconds = C.serverTimeEpsSeconds conf
    , sconfPort = C.serverPort conf
    , sconfPGConnection = con
    , sconfLogger = logger
    }

someFunc :: IO ()
someFunc = do
    let loadingLogger = L.stdHandler

    runOpts <- getOptsIO

    L.logDebug loadingLogger "run options are:"
    L.logDebug loadingLogger $ GP.textPretty runOpts

    apiKey <- fmap T.pack $ getEnv $ T.unpack $ apiConfigEnvVar runOpts
    L.logDebug loadingLogger $ "apiKey = " <> apiKey

    config <- C.loadConfig $ confPath runOpts
    L.logDebug loadingLogger $ GP.textPretty config

    L.logDebug loadingLogger "successfully got server configuration"

    when (testConfig runOpts) Q.exitSuccess

    let conStr = E.encodeUtf8 $ connectString config

    conFillers <- PS.connectPostgreSQL conStr
    conServer <- PS.connectPostgreSQL conStr
    let 
        loggerFilter = toLoggerFilter $ loggerSettings runOpts
        loggerConfigFillers = L.LoggerConfig {
            L.lcFilter = loggerFilter
            , L.lcPath = T.unpack $ logPathFillers runOpts
            }
        loggerConfigServer = L.LoggerConfig {
            L.lcFilter = loggerFilter
            , L.lcPath = T.unpack $ logPathServer runOpts
            }

    L.withSelfSufficientLogger loggerConfigFillers $ \loggerFillers ->
        L.withSelfSufficientLogger loggerConfigServer $ \loggerServer -> do

            let fillerConfig = toFillersConfig apiKey config conFillers loggerFillers
            startCacheFiller fillerConfig

            let serverConfig = toServerConfig apiKey config conServer loggerServer
            startServer serverConfig

