module App.FillerHandler where

import Prelude hiding (log)
import qualified App.Logger as L
import Types


data Environment = Environment {
    envLocationData :: LocationData
    , envFillerNumero :: Int
    } deriving (Show, Eq)

data FillerHandler m = FillerHandler {
    log :: L.LoggerHandler m
    , handlerEnv :: Environment
    , timeSinceEpoch :: m Integer
    , requestCurrentWeather :: LocationData -> m APIResponse
    , writeToCache :: APIResponse -> m ()
    , acquireDBLock :: m ()
    , giveAwayDBLock :: m ()
    , sleep :: m ()
    }
