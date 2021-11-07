{-# LANGUAGE RankNTypes #-}

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
        , timeSinceEpoch :: m Seconds
        , requestCurrentWeather :: LocationData -> m APIResponse
        , writeToCache :: APIResponse -> m ()
        , withLock :: forall a. m a -> m a
        , sleep :: m ()
        }
