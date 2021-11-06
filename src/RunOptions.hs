{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}

module RunOptions
    ( RunOptions(..)
    , getOptsIO
    , toLoggerFilter
    ) where

import qualified App.Logger as L
import qualified Data.Text as T
import GHC.Generics
import qualified GenericPretty as GP
import Options.Applicative

data LoggerSettings
    = LogAll
    | LogGreaterThan L.Priority
  deriving (Show, Eq)
  deriving GP.PrettyShow via GP.Showable LoggerSettings

toLoggerFilter :: LoggerSettings -> (L.Priority -> Bool)
toLoggerFilter LogAll = const True
toLoggerFilter (LogGreaterThan pri) = (>= pri)

data RunOptions =
    RunOptions
        { confPath :: T.Text
        , apiConfigEnvVar :: T.Text
        , migrations :: Bool
        , loggerSettings :: LoggerSettings
        , logPathFillers :: T.Text
        , logPathServer :: T.Text
        , testConfig :: Bool
        }
  deriving (Show, Eq, Generic)
  deriving anyclass (GP.PrettyShow)

--for ghci
ghciRunOpts :: RunOptions
ghciRunOpts =
    RunOptions
        { confPath = "./config.dhall"
        , apiConfigEnvVar = "OPEN_WEATHER_API_KEY"
        , migrations = False
        , loggerSettings = LogAll
        , logPathFillers = "./log_fillers"
        , logPathServer = "./log_server"
        , testConfig = False
        }

getOpts :: Parser RunOptions
getOpts =
    RunOptions <$> argument str (metavar "CONFIGPATH") <*>
    (argument str (metavar "API_KEY_ENV") <|>
     pure "OPEN_WEATHER_API_KEY") <*>
    switch
        (short 'm' <>
         long "migrations" <> help "Whether to run migrations") <*>
    ((getLoggerSettings <$>
      option
          auto
          (short 'l' <> metavar "LOGLEVEL" <> help "Log level")) <|>
     pure LogAll) <*>
    (strOption
         (long "logpathfillers" <>
          metavar "FILLERSLOGFILE" <>
          help "Log path for cache fillers") <|>
     pure "./log_fillers") <*>
    (strOption
         (long "logpathserver" <>
          metavar "SERVERLOGFILE" <> help "Log path for the server") <|>
     pure "./log_server") <*>
    switch (long "test-config" <> help "Test configuration")

--        <> help "Path to the dhall configuration file")
--        <> help "Environment variable containing OpenWeather API key"
serverHeader :: String
serverHeader = "Weather proxy-cache server for OpenWeather API"

getOptsIO :: IO RunOptions
getOptsIO =
    execParser $
    info (getOpts <**> helper) (fullDesc <> header serverHeader)

getLoggerSettings :: L.Priority -> LoggerSettings
getLoggerSettings = LogGreaterThan
