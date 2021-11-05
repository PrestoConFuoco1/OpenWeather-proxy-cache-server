module Server.Result where

import Types
import qualified App.ServerHandler as SH
import qualified Data.Text as T
import qualified Control.Monad.Catch as CMC
import qualified Utils as U

ok :: APIResponse -> Result
ok data_ = Result {
    resultOk = True
    , resultMessage = "data in \"result\" field"
    , resultData = Just data_
    }

failure :: T.Text -> Result
failure msg = Result {
    resultOk = False
    , resultMessage = msg
    , resultData = Nothing
    }


usage :: Result
usage = failure usageMessage

usageMessage = "time (integer, optional) - seconds since epoch; \
                \id - city_id (integer); id is required "

noDataFound :: Result
noDataFound = failure "no data found"

internalError :: Result
internalError = failure "internal error"
