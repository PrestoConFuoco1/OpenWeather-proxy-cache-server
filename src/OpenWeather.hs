{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}

module OpenWeather
    ( weatherByLocationData
    , OpenWeatherAPIError(..)
    , SC.ClientError(..)
    , SC.ResponseF(..)
    ) where

import Data.Proxy
import qualified Data.Text as T
import qualified GenericPretty as GP
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.API
import qualified Servant.Client as SC
import Types

import qualified Servant.Client.Streaming as S

import qualified Control.Monad.Catch as CMC

import qualified Data.Aeson as Ae
import Data.Aeson ((.:))
import DerivingJSON
import GHC.Generics
import Text.Read (readMaybe)

type OpenWeatherAPI
     = QueryParam "appid" T.Text :> QueryParam "id" Int :> Get '[ JSON] APIResponse :<|> QueryParam "appid" T.Text :> QueryParam "q" T.Text :> Get '[ JSON] APIResponse :<|> QueryParam "appid" T.Text :> QueryParam "lat" Double :> QueryParam "lon" Double :> Get '[ JSON] APIResponse

api :: Proxy OpenWeatherAPI
api = Proxy

baseScheme :: S.Scheme
baseScheme = SC.Http

baseUrlHost :: String
baseUrlHost = "api.openweathermap.org"

baseUrlPort :: Int
baseUrlPort = 80

baseUrlPath :: String
baseUrlPath = "data/2.5/weather"

openWeatherBaseUrl = S.BaseUrl baseScheme baseUrlHost baseUrlPort baseUrlPath

weatherByLocationIdClientM :<|> weatherByCityNameClientM :<|> weatherByCoordinatesClientM =
    SC.client api

toIOThrow :: SC.ClientM a -> IO a
toIOThrow action = do
    eithRes <- toIOEither action
    either CMC.throwM pure eithRes

toIOEither :: SC.ClientM a -> IO (Either SC.ClientError a)
toIOEither action = do
    manager <- newManager defaultManagerSettings
    SC.runClientM action $ SC.mkClientEnv manager openWeatherBaseUrl

weatherByLocationId :: T.Text -> Int -> IO APIResponse
weatherByLocationId key cityID =
    toIOThrow $ weatherByLocationIdClientM (Just key) (Just cityID)

weatherByCityName :: T.Text -> T.Text -> IO APIResponse
weatherByCityName key cityName =
    toIOThrow $ weatherByCityNameClientM (Just key) (Just cityName)

weatherByCoordinates :: T.Text -> Coordinates -> IO APIResponse
weatherByCoordinates key Coordinates {..} =
    toIOThrow $
    weatherByCoordinatesClientM
        (Just key)
        (Just $ unLatitude coordLat)
        (Just $ unLongitude coordLon)

weatherByLocationData :: T.Text -> LocationData -> IO APIResponse
weatherByLocationData key locationData =
    case locationData of
        LCityID cityID -> weatherByLocationId key cityID
        LCityName cityName -> weatherByCityName key cityName
        LCoords coords -> weatherByCoordinates key coords

data OpenWeatherAPIError =
    OpenWeatherAPIError
        { owapierrorCod :: Int
        , owapierrorMessage :: T.Text
        }
  deriving (Show, Eq, Generic)
  deriving GP.PrettyShow via PrefixCamel OpenWeatherAPIError

instance Ae.FromJSON OpenWeatherAPIError where
    parseJSON =
        Ae.withObject "open weather API error object" $ \o -> do
            msg <- o .: "message"
            codStr <- o .: "cod"
            let mCod = readMaybe (codStr :: String)
            cod <- maybe (fail "cod is not a number") pure mCod
            pure $
                OpenWeatherAPIError
                    {owapierrorCod = cod, owapierrorMessage = msg}
