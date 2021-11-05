{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module OpenWeather (
weatherByLocationData
, OpenWeatherAPIError(..)
, SC.ClientError (..)
, SC.ResponseF(..)
) where

import qualified GenericPretty as GP
import Data.Proxy
import qualified Data.Text as T
import qualified GenericPretty as GP
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.API
import qualified Servant.Client as SC
import Servant.Types.SourceT (foreach)
import Types

import qualified Servant.Client.Streaming as S

import qualified App.Logger as L
import Utils as U
import qualified Control.Monad.Catch as CMC

import qualified Data.Aeson as Ae
import Data.Aeson ((.:))
import Text.Read (readMaybe)
import DerivingJSON
import GHC.Generics


type OpenWeatherAPI
   = QueryParam "appid" T.Text :> QueryParam "id" Int :> Get '[ JSON] APIResponse

api :: Proxy OpenWeatherAPI
api = Proxy

weatherByLocationIdClient = SC.client api

baseScheme = SC.Http

baseUrlHost = "api.openweathermap.org"

baseUrlPort = 80

baseUrlPath = "data/2.5/weather"

openWeatherBaseUrl = S.BaseUrl baseScheme baseUrlHost baseUrlPort baseUrlPath

--weatherByLocationIdClientM
weatherByLocationIdClientM = SC.client api


toIOThrow :: SC.ClientM a -> IO a
toIOThrow action = do
    eithRes <- toIOEither action
    either CMC.throwM pure eithRes

toIOEither :: SC.ClientM a -> IO (Either SC.ClientError a)
toIOEither action = do
  manager <- newManager defaultManagerSettings
  SC.runClientM action $ SC.mkClientEnv manager openWeatherBaseUrl

weatherByLocationIdIOThrow ::
     Maybe T.Text -> Maybe Int -> IO APIResponse
weatherByLocationIdIOThrow key locationId = toIOThrow $ weatherByLocationIdClientM key locationId

weatherByLocationId :: T.Text -> Int -> IO APIResponse
weatherByLocationId key cityID = toIOThrow $ weatherByLocationIdClientM (Just key) (Just cityID)

weatherByLocationData :: T.Text -> LocationData -> IO APIResponse
weatherByLocationData key locationData = case locationData of
    LCityID cityID -> weatherByLocationId key cityID


data OpenWeatherAPIError = OpenWeatherAPIError {
    owapierrorCod :: Int
    , owapierrorMessage :: T.Text
    }
    deriving (Show, Eq, Generic)
    deriving GP.PrettyShow via PrefixCamel OpenWeatherAPIError
    --deriving (Ae.ToJSON, Ae.FromJSON) via PrefixCamel OpenWeatherAPIError

instance Ae.FromJSON OpenWeatherAPIError where
    parseJSON = Ae.withObject "open weather API error object" $ \o -> do
        msg <- o .: "message"
        codStr <- o .: "cod"
        let mCod = readMaybe (codStr :: String)
        cod <- maybe (fail "cod is not a number") pure mCod
        pure $ OpenWeatherAPIError {
            owapierrorCod = cod
            , owapierrorMessage = msg
            }
