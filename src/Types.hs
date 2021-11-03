{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}


module Types where

import Data.Aeson
import GHC.Generics
import qualified Data.Text as T
import DerivingJSON
import qualified GenericPretty as GP


-- all fields ok
data Coordinates = Coordinates {
    coordLon :: Double
    , coordLat :: Double
    }
    deriving stock (Show, Eq, Generic)
    deriving (ToJSON, FromJSON, GP.PrettyShow) via PrefixCamel Coordinates

-- all fields ok
data Weather = Weather {
    weatherID :: Maybe Int
    , weatherMain :: Maybe T.Text
    , weatherDescription :: Maybe T.Text
    , weatherIcon :: Maybe T.Text
    }
    deriving stock (Show, Eq, Generic)
    deriving (ToJSON, FromJSON, GP.PrettyShow) via PrefixCamel Weather

-- pressure and humidity are Integral in the examples
data Main = Main {
    mainTemp :: Maybe Double
    , mainFeelsLike :: Maybe Double
    , mainTempMin :: Maybe Double
    , mainTempMax :: Maybe Double
    , mainPressure :: Maybe Double
    , mainHumidity :: Maybe Double
    }
    deriving stock (Show, Eq, Generic)
    deriving (ToJSON, FromJSON, GP.PrettyShow) via PrefixCamel Main

-- all fields ok
data Wind = Wind {
    windSpeed :: Maybe Double
    , windDeg :: Maybe Double
    , windGust :: Maybe Double
    }
    deriving stock (Show, Eq, Generic)
    deriving (ToJSON, FromJSON, GP.PrettyShow) via PrefixCamel Wind

-- cloudsAll units are per-cent's
newtype Clouds = Clouds {
    cloudsAll :: Maybe Int
    }
    deriving stock (Show, Eq, Generic)
    deriving (ToJSON, FromJSON, GP.PrettyShow) via PrefixCamel Clouds

-- turned out to be double
data Rain = Rain {
    rain1H :: Maybe Double
    , rain3H :: Maybe Double
    }
    deriving stock (Show, Eq, Generic)
    deriving (ToJSON, FromJSON, GP.PrettyShow) via PrefixCamel Rain

-- turned out to be double
data Snow = Snow {
    snow1H :: Maybe Double
    , snow3H :: Maybe Double
    }
    deriving stock (Show, Eq, Generic)
    deriving (ToJSON, FromJSON, GP.PrettyShow) via PrefixCamel Snow

-- type and id are integral in the examples, others - ok
data Sys = Sys {
    sysType :: Maybe Int
    , sysId :: Maybe Int
    , sysMessage :: Maybe Double
    , sysCountry :: Maybe T.Text
    , sysSunrise :: Maybe Integer
    , sysSunset  :: Maybe Integer
    }
    deriving stock (Show, Eq, Generic)
    deriving (ToJSON, FromJSON, GP.PrettyShow) via PrefixCamel Sys

-- assumed that coord, main, dt (time since epoch),
-- timezone, id (city id) and cod are required, others - optionals
data APIResponse = APIResponse {
    apiCoord :: Coordinates
    , apiWeather :: Maybe [Weather]
    , apiBase :: Maybe T.Text
    , apiMain :: Main
    , apiWind :: Maybe Wind
    , apiClouds :: Maybe Clouds
    , apiRain :: Maybe Rain
    , apiSnow :: Maybe Snow
    , apiDt :: Integer
    , apiSys :: Maybe Sys
    , apiTimezone :: Int
    , apiId :: Int
    , apiName :: T.Text
    , apiCod :: Int
    }
    deriving stock (Show, Eq, Generic)
    deriving (ToJSON, FromJSON, GP.PrettyShow) via PrefixCamel APIResponse



