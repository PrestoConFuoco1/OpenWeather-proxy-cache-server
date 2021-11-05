module App.ServerHandler.Instance where

import App.ServerHandler
import qualified App.Logger as L
import Prelude hiding (log)
import qualified Utils as U
import qualified Database.PostgreSQL.Simple as PS
import qualified Database as D
import qualified OpenWeather as OW
import qualified Data.Text as T
import GHC.Generics

data Config = Config {
    configEnv :: Environment
    , configApiKey :: T.Text
    } deriving (Show, Eq, Generic)

newtype Resources =
    Resources
        { postgresConnection :: PS.Connection
        }

resourcesToHandle :: Config -> Resources -> L.LoggerHandler IO -> ServerHandler IO
resourcesToHandle conf (Resources con) logger =

    ServerHandler {

        handleEnv = configEnv conf
        , log = logger
        , timeSinceEpoch = U.secondsSinceEpoch
        , searchCache = D.searchCache con logger
        , requestCurrentWeather = OW.weatherByLocationData (configApiKey conf)
        , writeToCache = D.writeToCache con
        }


