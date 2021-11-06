module Lib
    ( someFunc
    ) where

import qualified App.Logger as L
import qualified Config as C
import Control.Monad (when)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Database.PostgreSQL.Simple as PS
import Filler.Start (FillerConfig(..), startCacheFiller)
import qualified GenericPretty as GP
import qualified Migrations as M
import RunOptions
import Server.Start (ServerConfig(..), startServer)
import System.Environment (getEnv)
import qualified System.Exit as Q
import qualified Control.Monad.Catch as CMC

connectString :: C.Config -> T.Text
connectString conf =
    "dbname=" <>
    databaseName <>
    " user=" <> userName <> " password='" <> password <> "'"
  where
    databaseName = C.databaseName conf
    userName = C.databaseUser conf
    password = C.databasePassword conf

toFillersConfig ::
       T.Text
    -> C.Config
    -> PS.Connection
    -> L.LoggerHandler IO
    -> FillerConfig
toFillersConfig apiKey conf con logger =
    FillerConfig
        { fconfCities = C.fillerCities conf
        , fconfApiKey = apiKey
        , fconfSleepTimeSeconds = C.fillerSleepTimeSeconds conf
        , fconfPGConnection = con
        , fconfLogger = logger
        }

toServerConfig ::
       T.Text
    -> C.Config
    -> PS.Connection
    -> L.LoggerHandler IO
    -> ServerConfig
toServerConfig apiKey conf con logger =
    ServerConfig
        { sconfApiKey = apiKey
        , sconfDelta = C.serverDelta conf
        , sconfPort = C.serverPort conf
        , sconfPGConnection = con
        , sconfLogger = logger
        }

toMigrationsConfig :: C.Config -> M.Config
toMigrationsConfig conf =
    M.Config
        { M.databaseName = C.databaseName conf
        , M.adminName = C.databaseUser conf
        , M.adminPassword = C.databasePassword conf
        }

someFunc :: IO ()
someFunc = do
    let loadingLogger = L.stdHandler
    runOpts <- getOptsIO
    L.logDebug loadingLogger "run options are:"
    L.logDebug loadingLogger $ GP.textPretty runOpts
    apiKey <-
        fmap T.pack $ getEnv $ T.unpack $ apiConfigEnvVar runOpts
    L.logDebug loadingLogger $ "apiKey = " <> apiKey
    config <- C.loadConfig $ confPath runOpts
    L.logDebug loadingLogger $ GP.textPretty config
    L.logDebug loadingLogger "successfully got server configuration"
    when (testConfig runOpts) Q.exitSuccess
    if migrations runOpts
        then M.migrationMain $ toMigrationsConfig config
        else run runOpts apiKey config

withPostgresConnection :: IO PS.Connection -> (PS.Connection -> IO ()) -> IO ()
withPostgresConnection getCon action =
    CMC.bracket getCon PS.close action 

run :: RunOptions -> T.Text -> C.Config -> IO ()
run runOpts apiKey config = do
    let conStr = E.encodeUtf8 $ connectString config
        connectAction = PS.connectPostgreSQL conStr
        withConnection = withPostgresConnection connectAction
    withConnection $ \conFillers ->
        withConnection $ \conServer ->
            runWithConnections conFillers conServer

  where
  runWithConnections conFillers conServer = do
    let loggerFilter = toLoggerFilter $ loggerSettings runOpts
        loggerConfigFillers =
            L.LoggerConfig
                { L.lcFilter = loggerFilter
                , L.lcPath = T.unpack $ logPathFillers runOpts
                }
        loggerConfigServer =
            L.LoggerConfig
                { L.lcFilter = loggerFilter
                , L.lcPath = T.unpack $ logPathServer runOpts
                }
    L.withSelfSufficientLogger loggerConfigFillers $ \loggerFillers ->
        L.withSelfSufficientLogger loggerConfigServer $ \loggerServer -> do
            let fillerConfig =
                    toFillersConfig
                        apiKey
                        config
                        conFillers
                        loggerFillers
            startCacheFiller fillerConfig
            let serverConfig =
                    toServerConfig
                        apiKey
                        config
                        conServer
                        loggerServer
            startServer serverConfig
