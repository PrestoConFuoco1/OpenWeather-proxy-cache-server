module App.FillerHandler.Instance where

import qualified App.FillerHandler as FH
import qualified App.Logger as L
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import qualified Data.Text as T
import qualified Database as D
import qualified Database.PostgreSQL.Simple as PS
import GHC.Generics
import qualified OpenWeather as OW
import Prelude hiding (log)
import qualified Utils as U

data Config =
    Config
        { configApiKey :: T.Text
        , sleepTimeSeconds :: Int
        }
  deriving (Show, Eq, Generic)

data Resources =
    Resources
        { postgresConnection :: PS.Connection
        , fillersLock :: MVar ()
        }

resourcesToHandle ::
       Config
    -> Resources
    -> L.LoggerHandler IO
    -> FH.Environment
    -> FH.FillerHandler IO
resourcesToHandle conf resources logger env =
    FH.FillerHandler
        { FH.log = logger
        , FH.handlerEnv = env
        , FH.timeSinceEpoch = U.secondsSinceEpoch
        , FH.requestCurrentWeather =
              OW.weatherByLocationData (configApiKey conf)
        , FH.writeToCache =
              D.writeToCache (postgresConnection resources)
        , FH.acquireLock = takeMVar (fillersLock resources)
        , FH.giveAwayLock = putMVar (fillersLock resources) ()
        , FH.sleep = threadDelay (sleepTimeSeconds conf * 1000000)
        }
