module App.ServerHandler where

import qualified App.Logger as L
import GHC.Generics
import Prelude hiding (log)
import Types

newtype Environment =
    Environment
        { envDelta :: Delta
        }
  deriving (Show, Eq, Generic)

getDelta :: Environment -> Delta
getDelta = envDelta

data ServerHandler m =
    ServerHandler
        { handleEnv :: Environment
        , log :: L.LoggerHandler m
        , timeSinceEpoch :: m Integer
        , searchCache :: Delta -> Integer -> LocationData -> m [APIResponse]
        , requestCurrentWeather :: LocationData -> m APIResponse
        , writeToCache :: APIResponse -> m ()
        }
