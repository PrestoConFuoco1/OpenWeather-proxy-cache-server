module App.ServerHandler.Instance
    ( Resources(..)
    , Config(..)
    , resourcesToHandle
    ) where

import qualified App.Logger as L
import App.ServerHandler
import qualified Data.Text as T
import qualified Database as D
import qualified Database.PostgreSQL.Simple as PS
import GHC.Generics
import qualified OpenWeather as OW
import Prelude hiding (log)
import qualified Utils as U

data Config =
    Config
        { configEnv :: Environment
        , configApiKey :: T.Text
        }
  deriving (Show, Eq, Generic)

newtype Resources =
    Resources
        { postgresConnection :: PS.Connection
        }

resourcesToHandle ::
       Config -> Resources -> L.LoggerHandler IO -> ServerHandler IO
resourcesToHandle conf (Resources con) logger =
    ServerHandler
        { handleEnv = configEnv conf
        , log = logger
        , timeSinceEpoch = U.secondsSinceEpoch
        , searchCache = D.searchCache con logger
        , requestCurrentWeather = OW.weatherByLocationData (configApiKey conf)
        , writeToCache = D.writeToCache con
        }
