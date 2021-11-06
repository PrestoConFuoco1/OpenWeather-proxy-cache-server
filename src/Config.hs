{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Config (Config(..), loadConfig) where

import GHC.Generics
import qualified Data.Text as T
import Dhall
import qualified GenericPretty as GP
import DerivingJSON

data ConfigDhall = ConfigDhall {
    dhallDatabaseName :: T.Text
    , dhallDatabaseUser :: T.Text
    , dhallDatabasePassword :: T.Text
    
    , dhallFillerCities :: [Natural]
    , dhallFillerSleepTimeSeconds :: Natural

    , dhallServerTimeEpsSeconds :: Natural
    , dhallServerPort :: Natural
    } deriving (Show, Eq, Generic)
instance FromDhall ConfigDhall


data Config = Config {
    databaseName :: T.Text
    , databaseUser :: T.Text
    , databasePassword :: T.Text
    
    , fillerCities :: [Int]
    , fillerSleepTimeSeconds :: Int

    , serverTimeEpsSeconds :: Integer
    , serverPort :: Int
    } deriving (Show, Eq, Generic)
    deriving GP.PrettyShow via PrefixCamel Config

fixConfig :: ConfigDhall -> Config
fixConfig confDhall = Config {
    databaseName = dhallDatabaseName confDhall
    , databaseUser = dhallDatabaseUser confDhall
    , databasePassword = dhallDatabasePassword confDhall

    , fillerCities = Prelude.map fromIntegral $ dhallFillerCities confDhall
    , fillerSleepTimeSeconds = fromIntegral $ dhallFillerSleepTimeSeconds confDhall

    , serverTimeEpsSeconds = fromIntegral $ dhallServerTimeEpsSeconds confDhall
    , serverPort = fromIntegral $ dhallServerPort confDhall
    }

loadConfigBad :: T.Text -> IO ConfigDhall
loadConfigBad path = -- do
    input auto path -- :: IO ConfigDhall

loadConfig :: T.Text -> IO Config
loadConfig path = fixConfig <$> loadConfigBad path
