module Exceptions where

import qualified Control.Monad.Catch as CMC
import qualified OpenWeather as OW
import qualified Data.Aeson as Ae
import qualified Data.Aeson.Types as AeT
import Types
import qualified App.Logger as L
import qualified Data.Text as T
import Server.Result
import qualified Utils as U

data ServerException =
    NoDataFound
    deriving (Show, Eq)

withExceptionHandlers ::
       (Foldable f, CMC.MonadCatch m)
    => f (CMC.Handler m a)
    -> m a
    -> m a
withExceptionHandlers = flip CMC.catches



instance CMC.Exception ServerException

throwNoDataFound :: (CMC.MonadThrow m) => m a
throwNoDataFound = CMC.throwM NoDataFound



errorHandlers :: (CMC.MonadCatch m) => L.LoggerHandler m -> [CMC.Handler m Result]
errorHandlers logger =
    [ CMC.Handler serverErrorHandler
    --, CMC.Handler $ defaultHandler logger
    ]

defaultHandler :: (CMC.MonadCatch m) => L.LoggerHandler m -> CMC.SomeException -> m Result
defaultHandler logger e = do
    L.logError logger "unexpected error occured"
    L.logError logger $ T.pack $ CMC.displayException e
    undefined
    

serverErrorHandler :: (CMC.MonadCatch m) => ServerException -> m Result
serverErrorHandler NoDataFound = pure noDataFound

{-
FailureResponse (Request {requestPath = (BaseUrl {baseUrlScheme = Http, baseUrlHost = "api.openweathermap.org",
baseUrlPort = 80, baseUrlPath = "data/2.5/weather"},""), requestQueryString =
fromList [("appid",Just "08a9fc08a909b0f3de55b60ce736fbfa"),("id",Just "4997171111111111")],
requestBody = Nothing, requestAccept = fromList [application/json;charset=utf-8,application/json],
requestHeaders = fromList []), requestHttpVersion = HTTP/1.1, requestMethod = "GET"}
(Response {responseStatusCode = Status {statusCode = 404, statusMessage = "Not Found"},
responseHeaders = fromList [("Server","openresty"),("Date","Fri, 05 Nov 2021 12:29:35 GMT"),
("Content-Type","application/json; charset=utf-8"),("Content-Length","40"),("Connection","keep-alive"),
("X-Cache-Key","/data/2.5/weather?id=4997171111111111"),("Access-Control-Allow-Origin","*"),
("Access-Control-Allow-Credentials","true"),("Access-Control-Allow-Methods","GET, POST")],
responseHttpVersion = HTTP/1.1, responseBody = "{\"cod\":\"404\",\"message\":\"city not found\"}"})
-}

openWeatherErrorHandler :: (CMC.MonadCatch m) => L.LoggerHandler m -> OW.ClientError -> m Result
openWeatherErrorHandler logger err@(OW.FailureResponse _ (OW.Response _ _ _ respBody)) = do
--openWeatherErrorHandler logger (OW.FailureResponse _ "hello") = do
    let
        maybeParsedBody = Ae.decode respBody >>= AeT.parseMaybe Ae.parseJSON :: Maybe OW.OpenWeatherAPIError
    U.withMaybe maybeParsedBody
        (failedToParseResponseError logger err)
        (pure . toAPIError)

toAPIError :: OW.OpenWeatherAPIError -> Result
toAPIError = undefined

failedToParseResponseError :: (Monad m) => L.LoggerHandler m -> OW.ClientError -> m Result
failedToParseResponseError logger err = do
    L.logError logger "failed to parse open weather response error"
    L.logError logger $ T.pack $ CMC.displayException err
    pure $ internalError



