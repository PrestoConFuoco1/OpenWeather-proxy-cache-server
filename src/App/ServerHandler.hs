module App.ServerHandler where

import Types
import qualified App.Logger as L
import Prelude hiding (log)
import GHC.Generics

data Environment = Environment {
    envDelta :: Delta
    } deriving (Show, Eq, Generic)

getDelta :: Environment -> Delta
getDelta = envDelta

data ServerHandler m = ServerHandler {
    handleEnv :: Environment
    , log :: L.LoggerHandler m
    , timeSinceEpoch :: m Integer
    , searchCache :: Delta -> Integer -> LocationData -> m [APIResponse]
    , requestCurrentWeather :: LocationData -> m APIResponse
    , writeToCache :: APIResponse -> m ()
    }


