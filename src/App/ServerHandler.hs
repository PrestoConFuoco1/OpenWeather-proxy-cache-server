module App.ServerHandler where

import Types
import qualified App.Logger as L
import Prelude hiding (log)
import qualified Data.Text as T
import GHC.Generics

data Environment = Environment {
    timeEps :: Integer
    } deriving (Show, Eq, Generic)

getTimeEps :: Environment -> Integer
getTimeEps = timeEps

data ServerHandler m = ServerHandler {
    handleEnv :: Environment
    , log :: L.LoggerHandler m
    , timeSinceEpoch :: m Integer
    , searchCache :: Integer -> Integer -> LocationData -> m [APIResponse]
    , requestCurrentWeather :: LocationData -> m APIResponse
    , writeToCache :: APIResponse -> m ()
    }


