{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Server where

import qualified Control.Monad.Catch as CMC
import qualified Data.Text as T
import Types
import Prelude hiding (log)
import qualified Utils as U
import qualified GenericPretty as GP
import Data.Maybe (listToMaybe)
import qualified App.ServerHandler as H
import qualified App.Logger as L
import qualified Exceptions as Ex
import Server.Result

getLocationData :: Maybe Int -> Maybe T.Text -> Maybe Double -> Maybe Double -> Maybe LocationData
getLocationData (Just cityID) _ _ _ = Just $ LCityID cityID
getLocationData _ (Just cityName) _ _ = Just $ LCityName cityName
getLocationData _ _ (Just lat) (Just lon) = Just $ LCoords {
    ldLat = lat
    , ldLon = lon
    }
getLocationData _ _ _ _ = Nothing

executeWithErrorHandlers :: (CMC.MonadCatch m) =>
    H.ServerHandler m -> Maybe Integer -> Maybe Int -> Maybe T.Text -> Maybe Double -> Maybe Double -> m Result
executeWithErrorHandlers h mTime mCityID mCityName mLat mLon =
    let mLocationData = getLocationData mCityID mCityName mLat mLon
        logger = H.log h
     in U.withMaybe mLocationData (pure usage) $ \locationData ->
            Ex.withExceptionHandlers (Ex.errorHandlers logger) $
                ok <$> execute h mTime locationData


execute :: (CMC.MonadCatch m) => H.ServerHandler m -> Maybe Integer -> LocationData -> m APIResponse
execute h mTime cityID = do
    let logger = H.log h
        env = H.handleEnv h
        --timeEps_ = H.getTimeEps env
        delta = H.getDelta env
    time <- maybe (H.timeSinceEpoch h) pure mTime
    L.logDebug logger "trying to obtain requested data from cache"
    data_ <- H.searchCache h delta time cityID
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
        delta = H.getDelta env
        timeEps_ = deltaTime delta
    L.logDebug logger "no data in cache found, data will be obtained from API the given time is current one"
    now <- H.timeSinceEpoch h
    let equals = equalsWithDelta timeEps_
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


