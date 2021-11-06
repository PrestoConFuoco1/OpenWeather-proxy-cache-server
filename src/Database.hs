{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
module Database where


import qualified Database.PostgreSQL.Simple as PS
import Types
import qualified Database.PostgreSQL.Simple.ToField as PSF
import GenericPretty
import qualified App.Logger as L
import qualified Utils as U
import qualified Data.Text as T

data SqlValue =
    forall a. (PSF.ToField a, Show a) =>
              SqlValue a

instance PSF.ToField SqlValue where
    toField (SqlValue x) = PSF.toField x

instance Show SqlValue where
    show (SqlValue x) = show x

instance PrettyShow SqlValue where
    prettyShow (SqlValue x) = prettyShow $ Showable x

-------------------------

insertQuery :: PS.Query
insertQuery =
  " INSERT INTO weather.cache VALUES\
    \(?, ?, \
    \ ? ,\
    \ ?, ?, ?, ?, \
    \ ?, ?, ?, ?, \
    \ ?, ?, ?, ?, ?, ?, \
    \ ?, ?, ?, \
    \ ?,   ?, ?,   ?, ?, \
    \ ?, ?, ?, ?, ?, ? ) ON CONFLICT DO NOTHING "

selectQueryByCityID :: Delta -> Integer -> Int -> (PS.Query, [SqlValue])
selectQueryByCityID delta time cityID =
    --let qu = "SELECT * FROM weather.cache WHERE city_id = ? AND dt BETWEEN ? AND ?"
    let qu = "SELECT * FROM weather.cache WHERE dt BETWEEN ? AND ? AND city_id = ?"
        timeEps = deltaTime delta
        minTime = time - timeEps
        maxTime = time + timeEps
        pars = [SqlValue minTime, SqlValue maxTime, SqlValue cityID]
     in (qu, pars)

selectQueryByCityName :: Delta -> Integer -> T.Text -> (PS.Query, [SqlValue])
selectQueryByCityName delta time cityName =
    let qu = "SELECT * FROM weather.cache WHERE dt BETWEEN ? AND ? AND city_name = ?"
        timeEps = deltaTime delta
        (minTime, maxTime) = (time - timeEps, time + timeEps)
        pars = [SqlValue minTime, SqlValue maxTime, SqlValue cityName]
     in (qu, pars)

selectQueryByCoordinates :: Delta -> Integer -> Double -> Double -> (PS.Query, [SqlValue])
selectQueryByCoordinates delta time lat lon = 
    let qu = "SELECT * FROM weather.cache WHERE dt BETWEEN ? AND ? \
             \ AND coord_latitude BETWEEN ? AND ? \
             \ AND coord_longitude BETWEEN ? AND ? "
        timeEps = deltaTime delta
        (minTime, maxTime) = (time - timeEps, time + timeEps)
        latEps = deltaLat delta
        (minLat, maxLat) = (lat - latEps, lat + latEps)
        lonEps = deltaLon delta
        (minLon, maxLon) = (lon - lonEps, lon + lonEps)
        pars =  [ SqlValue minTime, SqlValue maxTime
                , SqlValue minLat, SqlValue maxLat
                , SqlValue minLon, SqlValue maxLon]
     in (qu, pars)

selectQueryByLocationData :: Delta -> Integer -> LocationData -> (PS.Query, [SqlValue])
selectQueryByLocationData delta time locationData = case locationData of
    LCityID cityID -> selectQueryByCityID delta time cityID
    LCityName cityName -> selectQueryByCityName delta time cityName
    LCoords {..} -> selectQueryByCoordinates delta time ldLat ldLon

searchCache :: PS.Connection -> L.LoggerHandler IO -> Delta -> Integer -> LocationData -> IO [APIResponse]
searchCache con logger delta time locationData = do
    let (qu, pars) = selectQueryByLocationData delta time locationData
    quBS <- PS.formatQuery con qu pars
    L.logDebug logger $ U.showText quBS
    PS.query con qu pars

writeToCache :: PS.Connection -> APIResponse -> IO ()
writeToCache con currentWeather = do
    _ <- PS.execute con insertQuery currentWeather
    pure ()


