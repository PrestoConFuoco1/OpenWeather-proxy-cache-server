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

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _ = Nothing

secondsSinceEpoch :: IO Integer
secondsSinceEpoch = fromIntegral . Sys.systemSeconds <$> Sys.getSystemTime

debugTime :: IO Time.UTCTime
debugTime = Sys.systemToUTCTime <$> Sys.getSystemTime
