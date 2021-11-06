module Server.Result where

import qualified Data.Text as T
import Types

codeOk, codeBad, codeInternal :: Int
codeOk = 200

codeBad = 400

codeInternal = 500

ok :: APIResponse -> Result
ok data_ =
    Result
        { resultOk = True
        , resultMessage = "data in \"result\" field"
        , resultData = Just data_
        , resultCod = codeOk
        }

failure :: Int -> T.Text -> Result
failure code msg =
    Result
        { resultOk = False
        , resultMessage = msg
        , resultData = Nothing
        , resultCod = code
        }

usage :: Result
usage = failure codeBad usageMessage

usageMessage :: T.Text
usageMessage =
    "time (integer, optional) - seconds since epoch;\n \
    \ city_id (integer) - city OpenWeather API identifier (integer);\n \
    \ city_name (string) - city name :)\n \
    \ latitude (double), longitude (double)\n \
    \ correct combinations of location parameters:\n \
    \ 1) city_id\n \
    \ 2) city_name\n \
    \ 3) latitude, longitude "

noDataFound :: Result
noDataFound = failure codeOk "no data found"

internalError :: Result
internalError = failure codeInternal "internal error"
