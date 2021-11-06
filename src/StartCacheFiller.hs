module StartCacheFiller (startCacheFiller, FillerConfig(..)) where

import qualified App.FillerHandler.Instance as AFI
import Control.Concurrent.MVar (newMVar)
import Types
import qualified App.Logger as L
import Filler (fillerFlow)
import qualified App.FillerHandler as FH
import Control.Concurrent (forkIO)
import qualified Database.PostgreSQL.Simple as PS
import qualified Data.Text as T


data FillerConfig = FillerConfig {
    fconfPGConnection :: PS.Connection
    , fconfLogger :: L.LoggerHandler IO

    , fconfCities :: [Int]
    , fconfApiKey :: T.Text
    , fconfSleepTimeSeconds :: Int
    }



startCacheFiller :: FillerConfig -> IO ()
startCacheFiller fillerConfig = do

    lockMVar <- newMVar ()
    let 
        fillerHandlerConfig = AFI.Config {
            AFI.configApiKey = fconfApiKey fillerConfig
            , AFI.sleepTimeSeconds = fconfSleepTimeSeconds fillerConfig
            }
        con = fconfPGConnection fillerConfig
        logger = fconfLogger fillerConfig

        resources = AFI.Resources con lockMVar

        buildHandle = AFI.resourcesToHandle fillerHandlerConfig resources logger
        locationDataList = map LCityID $ fconfCities fillerConfig
        envs = zipWith FH.Environment locationDataList [1..]
        handles = map buildHandle envs

    mapM_ (forkIO . fillerFlow) handles

