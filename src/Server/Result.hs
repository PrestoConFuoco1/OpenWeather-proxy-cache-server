module Server.Result where

import Types
import qualified App.ServerHandler as SH
import qualified Data.Text as T
import qualified Control.Monad.Catch as CMC
import qualified Utils as U

codeOk = 200
codeBad = 400
codeInternal = 500

ok :: APIResponse -> Result
ok data_ = Result {
    resultOk = True
    , resultMessage = "data in \"result\" field"
    , resultData = Just data_
    , resultCod = codeOk
    }

failure :: Int -> T.Text -> Result
failure code msg = Result {
    resultOk = False
    , resultMessage = msg
    , resultData = Nothing
    , resultCod = code
    }


usage :: Result
usage = failure codeBad usageMessage

usageMessage = "time (integer, optional) - seconds since epoch; \
                \id - city_id (integer); id is required "

noDataFound :: Result
noDataFound = failure codeOk "no data found"

internalError :: Result
internalError = failure codeInternal "internal error"
