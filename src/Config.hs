{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Config
    ( Config(..)
    , loadConfig
    ) where

import qualified Data.Text as T
import DerivingJSON
import Dhall
import GHC.Generics
import qualified GenericPretty as GP
import Types

data ConfigDhall =
    ConfigDhall
        { dhallDatabaseName :: T.Text
        , dhallDatabaseUser :: T.Text
        , dhallDatabasePassword :: T.Text
        , dhallFillerCities :: [Natural]
        , dhallFillerSleepTimeSeconds :: Natural
        , dhallServerTimeEpsSeconds :: Natural
        , dhallServerLatEps :: Double
        , dhallServerLonEps :: Double
        , dhallServerPort :: Natural
        }
  deriving (Show, Eq, Generic)

instance FromDhall ConfigDhall

data Config =
    Config
        { databaseName :: T.Text
        , databaseUser :: T.Text
        , databasePassword :: T.Text
        , fillerCities :: [Int]
        , fillerSleepTimeSeconds :: Int
        , serverDelta :: Delta
        , serverPort :: Int
        }
  deriving (Show, Eq, Generic)
  deriving GP.PrettyShow via PrefixCamel Config

fixConfig :: ConfigDhall -> Config
fixConfig confDhall =
    Config
        { databaseName = dhallDatabaseName confDhall
        , databaseUser = dhallDatabaseUser confDhall
        , databasePassword = dhallDatabasePassword confDhall
        , fillerCities =
              Prelude.map fromIntegral $ dhallFillerCities confDhall
        , fillerSleepTimeSeconds =
              fromIntegral $ dhallFillerSleepTimeSeconds confDhall
        , serverDelta =
              Delta
                  { deltaTime =
                        fromIntegral $
                        dhallServerTimeEpsSeconds confDhall
                  , deltaLat = dhallServerLatEps confDhall
                  , deltaLon = dhallServerLonEps confDhall
                  }
        , serverPort = fromIntegral $ dhallServerPort confDhall
        }

loadConfigBad :: T.Text -> IO ConfigDhall
loadConfigBad = input auto -- :: IO ConfigDhall

loadConfig :: T.Text -> IO Config
loadConfig path = fixConfig <$> loadConfigBad path
