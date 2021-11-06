module Filler where


import qualified App.FillerHandler as FH
import qualified Utils as U
import qualified App.Logger as L
import qualified GenericPretty as GP
import Types
import qualified Control.Monad.Catch as CMC
import Control.Monad (forever)
import qualified Data.Text as T

fillerFlow :: (CMC.MonadCatch m) => FH.FillerHandler m -> m ()
fillerFlow h = forever $ do
    let logger = FH.log h
    CMC.handle
        (fillerErrorHandler logger)
        (fillerLoop h)
    FH.sleep h

fillerErrorHandler :: (CMC.MonadCatch m) => L.LoggerHandler m -> CMC.SomeException -> m ()
fillerErrorHandler logger e = do
    L.logError logger "filler: unexpected error occured"
    L.logError logger $ T.pack $ "filler: " <> CMC.displayException e


fillerLoop :: (CMC.MonadCatch m) => FH.FillerHandler m -> m ()
fillerLoop h = do
    let logger = FH.log h
        env = FH.handlerEnv h
        locationData = FH.envLocationData env
        num = FH.envFillerNumero env
    FH.acquireDBLock h
    L.logDebug logger $ withNum h $ "trying to get data from OpenWeather API"
    L.logDebug logger $ withNum h $ "city ID = " <> (U.showText locationData)
    currentWeather <- FH.requestCurrentWeather h locationData
    L.logDebug logger $ withNum h $ "got current weather for location " <> U.showText locationData
    L.logDebug logger $ GP.textPretty currentWeather
    currentTime <- FH.timeSinceEpoch h
    L.logDebug logger $ withNum h $ "writing obtained data into cache for time = " <> U.showText currentTime
    L.logDebug logger $ withNum h $ "trying to acquire lock"
    L.logDebug logger $ withNum h $ "acquired lock"
    FH.writeToCache h currentWeather
    L.logDebug logger $ withNum h $ "lock free; ok"
    FH.giveAwayDBLock h

withNum :: FH.FillerHandler m -> T.Text -> T.Text
withNum h text =
    let prompt = "filler " <> U.showText (FH.envFillerNumero $ FH.handlerEnv h) <> ": "
     in prompt <> text
