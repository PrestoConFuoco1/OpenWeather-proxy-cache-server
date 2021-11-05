{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Server where

import qualified Control.Monad.Catch as CMC
import qualified Data.Text as T
import Types
import GHC.Generics
import Prelude hiding (log)
import qualified Utils as U
import qualified GenericPretty as GP
import DerivingJSON
import qualified Data.Aeson as Ae
import Data.Maybe (listToMaybe)
import qualified App.ServerHandler as H
import qualified App.Logger as L
import qualified Exceptions as Ex
import Server.Result
import qualified Utils as U

getLocationData :: Maybe Int -> Maybe LocationData
getLocationData (Just cityID) = Just $ LCityID cityID

executeWithErrorHandlers :: (CMC.MonadCatch m) =>
    H.ServerHandler m -> Maybe Integer -> Maybe Int -> m Result
executeWithErrorHandlers h mTime mCityID =
    let mLocationData = getLocationData mCityID
        logger = H.log h
     in U.withMaybe mLocationData (pure usage) $ \locationData ->
        Ex.withExceptionHandlers (Ex.errorHandlers logger) $
            ok <$> execute h mTime locationData


execute :: (CMC.MonadCatch m) => H.ServerHandler m -> Maybe Integer -> LocationData -> m APIResponse
execute h mTime cityID = do
    let logger = H.log h
        env = H.handleEnv h
        timeEps_ = H.getTimeEps env
    time <- maybe (H.timeSinceEpoch h) pure mTime
    L.logDebug logger "trying to obtain requested data from cache"
    data_ <- H.searchCache h timeEps_ time cityID
    U.withMaybe (listToMaybe data_) (noDataInCache h time cityID) $ \fromCache -> do
        L.logDebug logger $ "ok, found info in the cache: "
        L.logDebug logger $ GP.textPretty fromCache
        pure fromCache

between :: (Ord a) => a -> (a, a) -> Bool
between val (minB, maxB) = val <= maxB && val >= minB

{-
-}
equalsWithDelta :: (Ord a, Num a) => a -> a -> a -> Bool
equalsWithDelta delta x y = abs (x - y) <= delta

noDataInCache :: (CMC.MonadCatch m) => H.ServerHandler m -> Integer -> LocationData -> m APIResponse
noDataInCache h time cityID = do
    let logger = H.log h
        env = H.handleEnv h
        timeEps' = H.getTimeEps env
    L.logDebug logger "no data in cache found, data will be obtained from API the given time is current one"
    now <- H.timeSinceEpoch h
    let equals = equalsWithDelta timeEps'
    if time `equals` now
        then do
            L.logDebug logger "asking openweather api for the current weather"
            data_ <- askOpenWeatherAPI h cityID
            L.logDebug logger "obtained some data"
            L.logDebug logger $ GP.textPretty data_
            pure data_
        else do
            L.logDebug logger "no data found for the past moment of time"
            Ex.throwNoDataFound

{-
noData :: Result
noData = Result False "no data found" Nothing
-}  

askOpenWeatherAPI :: (CMC.MonadCatch m) => H.ServerHandler m -> LocationData -> m APIResponse
askOpenWeatherAPI h cityID = do
    let logger = H.log h
    L.logDebug logger "trying to get weather data from OpenWeather API"
    currentWeather <- H.requestCurrentWeather h cityID
    H.writeToCache h currentWeather
    pure currentWeather


