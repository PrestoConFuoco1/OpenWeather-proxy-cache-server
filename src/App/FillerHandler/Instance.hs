module App.FillerHandler.Instance where

import qualified App.FillerHandler as FH
import qualified App.Logger as L
import Prelude hiding (log)
import qualified Utils as U
import qualified Database.PostgreSQL.Simple as PS
import qualified Database as D
import qualified OpenWeather as OW
import qualified Data.Text as T
import GHC.Generics
import Control.Concurrent.MVar
import Control.Concurrent (threadDelay)
import qualified Database as D
import qualified OpenWeather as OW

data Config = Config {
    configApiKey :: T.Text
    , sleepTimeSeconds :: Int
    --configEnv :: Environment
    } deriving (Show, Eq, Generic)


data Resources = Resources {
    postgresConnection :: PS.Connection
    , databaseLock :: MVar ()
    }


resourcesToHandle :: Config -> Resources -> L.LoggerHandler IO -> FH.Environment -> FH.FillerHandler IO
resourcesToHandle conf resources logger env =
    FH.FillerHandler {
        FH.log = logger
        , FH.handlerEnv = env
        , FH.timeSinceEpoch = U.secondsSinceEpoch
        , FH.requestCurrentWeather = OW.weatherByLocationData (configApiKey conf)
        , FH.writeToCache = D.writeToCache (postgresConnection resources)
        , FH.acquireDBLock = takeMVar (databaseLock resources)
        , FH.giveAwayDBLock = putMVar (databaseLock resources) ()
        , FH.sleep = threadDelay (sleepTimeSeconds conf * 1000000)
        }
