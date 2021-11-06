module App.FillerHandler where

import qualified App.Logger as L
import Prelude hiding (log)
import Types

data Environment =
    Environment
        { envLocationData :: LocationData
        , envFillerNumero :: Int
        }
  deriving (Show, Eq)

data FillerHandler m =
    FillerHandler
        { log :: L.LoggerHandler m
        , handlerEnv :: Environment
        , timeSinceEpoch :: m Integer
        , requestCurrentWeather :: LocationData -> m APIResponse
        , writeToCache :: APIResponse -> m ()
        , acquireLock :: m ()
        , giveAwayLock :: m ()
        , sleep :: m ()
        }
