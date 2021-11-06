{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}

module Types where

import qualified Data.Aeson as Ae
import Data.Maybe (maybeToList)
import qualified Data.Text as T
import Database.PostgreSQL.Simple ((:.)(..), Only(..))
import qualified Database.PostgreSQL.Simple.FromRow as PSF (FromRow(..), field)
import qualified Database.PostgreSQL.Simple.ToRow as PST (ToRow(..))
import DerivingJSON
import GHC.Generics
import qualified GenericPretty as GP
import qualified Utils as U

class HasNothing a where
  allNothing :: a

maybeAllNothing :: HasNothing a => Maybe a -> a
maybeAllNothing = maybe allNothing id

allNothingMaybe :: (Eq a, HasNothing a) => a -> Maybe a
allNothingMaybe x
  | x == allNothing = Nothing
  | otherwise = Just x

-- all fields ok
data Coordinates =
  Coordinates
    { coordLon :: Double
    , coordLat :: Double
    }
  deriving  (Show, Eq, Generic)
  deriving anyclass (PST.ToRow, PSF.FromRow)
  deriving (Ae.ToJSON, Ae.FromJSON, GP.PrettyShow) via PrefixCamel Coordinates

-- all fields ok
data Weather =
  Weather
    { weatherID :: Maybe Int
    , weatherMain :: Maybe T.Text
    , weatherDescription :: Maybe T.Text
    , weatherIcon :: Maybe T.Text
    }
  deriving  (Show, Eq, Generic)
  deriving anyclass (PST.ToRow, PSF.FromRow)
  deriving (Ae.ToJSON, Ae.FromJSON, GP.PrettyShow) via PrefixCamel Weather

nothingWeather :: Weather
nothingWeather = Weather Nothing Nothing Nothing Nothing

instance HasNothing Weather where
  allNothing = nothingWeather

-- pressure and humidity are Integral in the examples
data Main =
  Main
    { mainTemp :: Maybe Double
    , mainFeelsLike :: Maybe Double
    , mainTempMin :: Maybe Double
    , mainTempMax :: Maybe Double
    , mainPressure :: Maybe Double
    , mainHumidity :: Maybe Double
    }
  deriving  (Show, Eq, Generic)
  deriving anyclass (PST.ToRow, PSF.FromRow)
  deriving (Ae.ToJSON, Ae.FromJSON, GP.PrettyShow) via PrefixCamel Main

nothingMain :: Main
nothingMain = Main Nothing Nothing Nothing Nothing Nothing Nothing

instance HasNothing Main where
  allNothing = nothingMain

-- all fields ok
data Wind =
  Wind
    { windSpeed :: Maybe Double
    , windDeg :: Maybe Double
    , windGust :: Maybe Double
    }
  deriving  (Show, Eq, Generic)
  deriving anyclass (PST.ToRow, PSF.FromRow)
  deriving (Ae.ToJSON, Ae.FromJSON, GP.PrettyShow) via PrefixCamel Wind

nothingWind :: Wind
nothingWind = Wind Nothing Nothing Nothing

instance HasNothing Wind where
  allNothing = nothingWind

-- cloudsAll units are per-cent's
newtype Clouds =
  Clouds
    { cloudsAll :: Maybe Int
    }
  deriving  (Show, Eq, Generic)
  deriving anyclass (PST.ToRow, PSF.FromRow)
  deriving (Ae.ToJSON, Ae.FromJSON, GP.PrettyShow) via PrefixCamel Clouds

nothingClouds :: Clouds
nothingClouds = Clouds Nothing

instance HasNothing Clouds where
  allNothing = nothingClouds

-- turned out to be double
data Rain =
  Rain
    { rain1H :: Maybe Double
    , rain3H :: Maybe Double
    }
  deriving  (Show, Eq, Generic)
  deriving anyclass (PST.ToRow, PSF.FromRow)
  deriving (Ae.ToJSON, Ae.FromJSON, GP.PrettyShow) via PrefixCamel Rain

nothingRain :: Rain
nothingRain = Rain Nothing Nothing

instance HasNothing Rain where
  allNothing = nothingRain

-- turned out to be double
data Snow =
  Snow
    { snow1H :: Maybe Double
    , snow3H :: Maybe Double
    }
  deriving  (Show, Eq, Generic)
  deriving anyclass (PST.ToRow, PSF.FromRow)
  deriving (Ae.ToJSON, Ae.FromJSON, GP.PrettyShow) via PrefixCamel Snow

nothingSnow :: Snow
nothingSnow = Snow Nothing Nothing

instance HasNothing Snow where
  allNothing = nothingSnow

-- type and id are integral in the examples, others - ok
data Sys =
  Sys
    { sysType :: Maybe Int
    , sysId :: Maybe Int
    , sysMessage :: Maybe Double
    , sysCountry :: Maybe T.Text
    , sysSunrise :: Maybe Integer
    , sysSunset :: Maybe Integer
    }
  deriving  (Show, Eq, Generic)
  deriving anyclass (PST.ToRow, PSF.FromRow)
  deriving (Ae.ToJSON, Ae.FromJSON, GP.PrettyShow) via PrefixCamel Sys

nothingSys :: Sys
nothingSys = Sys Nothing Nothing Nothing Nothing Nothing Nothing

instance HasNothing Sys where
  allNothing = nothingSys

-- assumed that coord, main, dt (time since epoch),
-- timezone, id (city id) and cod are required, others - optionals
data APIResponse =
  APIResponse
    { apiCoord :: Coordinates -- main field
    , apiDt :: Integer -- main field
    , apiBase :: Maybe T.Text
    , apiTimezone :: Int
    , apiId :: Int
    , apiName :: T.Text
    , apiWeather :: [Weather]
    --, apiWeather :: Maybe Weather
    , apiMain :: Maybe Main
    , apiWind :: Maybe Wind
    , apiClouds :: Maybe Clouds
    , apiRain :: Maybe Rain
    , apiSnow :: Maybe Snow
    , apiSys :: Maybe Sys
    }
  deriving  (Show, Eq, Generic)

--    , apiCod :: Int
  deriving (Ae.ToJSON, Ae.FromJSON, GP.PrettyShow) via PrefixCamel APIResponse

instance PST.ToRow APIResponse where
  toRow = apiResponseToRow

apiResponseToRow APIResponse {..} =
  PST.toRow $
  apiCoord :. Only apiDt :. Only apiBase :. Only apiTimezone :. Only apiId :.
  Only apiName :.
  maybeAllNothing (U.safeHead apiWeather) :.
  maybeAllNothing apiMain :.
  maybeAllNothing apiWind :.
  maybeAllNothing apiClouds :.
  maybeAllNothing apiRain :.
  maybeAllNothing apiSnow :.
  maybeAllNothing apiSys

instance PSF.FromRow APIResponse where
  fromRow = apiResponseFromRow

apiResponseFromRow = do
  apiCoord <- PSF.fromRow
  apiDt <- PSF.field
  apiBase <- PSF.field
  apiTimezone <- PSF.field
  apiId <- PSF.field
  apiName <- PSF.field
  apiWeather <- (maybeToList . allNothingMaybe) <$> PSF.fromRow
  apiMain <- allNothingMaybe <$> PSF.fromRow
  apiWind <- allNothingMaybe <$> PSF.fromRow
  apiClouds <- allNothingMaybe <$> PSF.fromRow
  apiRain <- allNothingMaybe <$> PSF.fromRow
  apiSnow <- allNothingMaybe <$> PSF.fromRow
  apiSys <- allNothingMaybe <$> PSF.fromRow
  return APIResponse {..}

data Result = Result {
    resultOk :: Bool
    , resultMessage :: T.Text
    , resultData :: Maybe APIResponse
    , resultCod :: Int
    }
    deriving (Show, Eq, Generic)
    deriving (Ae.ToJSON, Ae.FromJSON, GP.PrettyShow) via PrefixCamel Result


data LocationData =
    LCityID { unLCityID :: Int }
    | LCityName { unLCityName :: T.Text }
    | LCoords {
        ldLat :: Double
        , ldLon :: Double
        }
    deriving (Show, Eq, Generic)

data Delta = Delta {
    deltaTime :: Integer
    , deltaLat :: Double
    , deltaLon :: Double
    } deriving (Show, Eq, Generic)
    deriving GP.PrettyShow via PrefixCamel Delta

