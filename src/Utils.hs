module Utils where

import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Data.Time.Clock.System as Sys

showText :: (Show a) => a -> Text.Text
showText = Text.pack . show

withMaybe :: Maybe a -> b -> (a -> b) -> b
withMaybe x nothing just = maybe nothing just x

withEither :: Either e a -> (e -> b) -> (a -> b) -> b
withEither val left right = either left right val

showDay :: Time.Day -> String
showDay = Time.formatTime Time.defaultTimeLocale "%F"

readDay :: String -> Maybe Time.Day
readDay = Time.parseTimeM True Time.defaultTimeLocale "%Y-%-m-%-d"

showUTCTime :: Time.UTCTime -> String
showUTCTime = Time.formatTime Time.defaultTimeLocale "%F %T.%4q UTC"

showUTCTimeText = Text.pack . showUTCTime

{-
showUTCTime2 = Time.formatTime Time.defaultTimeLocale "%F %T.%q"

debug :: Time.UTCTime -> (String, String)
debug time = (showUTCTime time, showUTCTime2 time)
-}

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _ = Nothing

secondsSinceEpoch :: IO Int
secondsSinceEpoch = fromIntegral . Sys.systemSeconds <$> Sys.getSystemTime

utcTime :: IO Time.UTCTime
utcTime = Sys.systemToUTCTime <$> Sys.getSystemTime
