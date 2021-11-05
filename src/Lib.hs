module Lib
  ( someFunc
  ) where

import StartServer (startServer, ServerConfig(..))
import StartCacheFiller (startCacheFiller, FillerConfig(..))
import Database (connectMyDB)
import qualified App.Logger as L
import qualified Database.PostgreSQL.Simple as PS
import Private (apiKey)


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
    , fconfApiKey = apiKey
    , fconfSleepTimeSeconds = 60*10
    , fconfPGConnection = con
    , fconfLogger = logger
    }


defaultServerPort = 8081
defaultTimeEpsSeconds = 1000

defaultServerConfig :: PS.Connection -> L.LoggerHandler IO -> ServerConfig
defaultServerConfig con logger = ServerConfig {
    sconfPGConnection = con
    , sconfLogger = logger
    , sconfApiKey = apiKey
    , sconfTimeEpsSeconds = defaultTimeEpsSeconds
    , sconfPort = defaultServerPort
    }


someFunc = do
    conFillers <- connectMyDB
    let fillerConfig = defaultFillerConfig conFillers L.stdHandler
    startCacheFiller fillerConfig

    conServer <- connectMyDB
    let serverConfig = defaultServerConfig conServer L.stdHandler
    startServer serverConfig

