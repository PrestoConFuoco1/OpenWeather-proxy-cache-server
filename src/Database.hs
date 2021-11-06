{-# LANGUAGE ExistentialQuantification #-}
module Database where


import qualified Database.PostgreSQL.Simple as PS
import Types
import qualified Database.PostgreSQL.Simple.ToField as PSF
import GenericPretty
import qualified App.Logger as L
import qualified Utils as U

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

selectQueryByCityID :: Integer -> Integer -> Int -> (PS.Query, [SqlValue])
selectQueryByCityID timeEps time cityID =
    let qu = "SELECT * FROM weather.cache WHERE city_id = ? AND dt BETWEEN ? AND ?"
        minTime = time - timeEps
        maxTime = time + timeEps
        pars = [SqlValue cityID, SqlValue minTime, SqlValue maxTime]
     in (qu, pars)



searchCacheByCityID :: PS.Connection -> L.LoggerHandler IO -> Integer -> Integer -> Int -> IO [APIResponse]
searchCacheByCityID con logger timeEps time cityID = do
    let (qu, pars) = selectQueryByCityID timeEps time cityID
    quBS <- PS.formatQuery con qu pars
    L.logDebug logger $ U.showText quBS
    PS.query con qu pars

searchCache :: PS.Connection -> L.LoggerHandler IO -> Integer -> Integer -> LocationData -> IO [APIResponse]
searchCache con logger timeEps time locationData = case locationData of
    LCityID cityID -> searchCacheByCityID con logger timeEps time cityID

writeToCache :: PS.Connection -> APIResponse -> IO ()
writeToCache con currentWeather = do
    _ <- fromIntegral <$> PS.execute con insertQuery currentWeather
    pure ()


