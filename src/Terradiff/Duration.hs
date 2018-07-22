{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
-- | Go-style Durations.
--
-- Durations have a simple, standard format.

module Terradiff.Duration
  ( Duration(..)
  , toDiffTime
  , formatDuration
  , parseDuration
  , durationParser
  , mul
  , nanosecond
  , microsecond
  , millisecond
  , second
  , minute
  , hour
  , sleep
  ) where

import Protolude hiding (second)

import Data.Aeson (FromJSON(..), ToJSON(..), withText)
import Data.Attoparsec.Text ((<?>), Parser, double, endOfInput, signed, parseOnly)
import Data.Time.Clock (DiffTime, picosecondsToDiffTime)
import Data.String (String)

-- | Delay a thread for a Duration.
sleep :: Duration -> IO ()
sleep duration = threadDelay (round ((fromIntegral (toNanoseconds duration) :: Double) / 1000))

-- | A duration in time.
--
-- Nothing sophisticated, rather this is intended to be a simple wrapper that
-- implements Go-style duration parsing.
newtype Duration = Duration { toNanoseconds :: Int64 } deriving (Eq, Ord, Show, Num)

instance ToJSON Duration where
  toJSON = toJSON . formatDuration

instance FromJSON Duration where
  parseJSON = withText "Duration must be text" $ \text ->
    case parseDuration text of
      Nothing -> empty
      Just duration -> pure duration

-- | Convert a 'Duration' to 'DiffTime'.
toDiffTime :: Duration -> DiffTime
toDiffTime = picosecondsToDiffTime . (1000 *) .  fromIntegral . toNanoseconds

-- | Scalar multiply a 'Duration'.
mul :: RealFrac a => a -> Duration -> Duration
mul n (Duration m) = Duration (round (n * fromIntegral m))


-- | Various units of time.
nanosecond, microsecond, millisecond, second, minute, hour :: Duration
nanosecond = Duration 1
microsecond = 1000 * nanosecond
millisecond = 1000 * microsecond
second = 1000 * millisecond
minute = 60 * second
hour = 60 * minute


-- | Parse a 'Duration'.
parseDuration :: Alternative f => Text -> f Duration
parseDuration = hush . parseOnly (durationParser <* endOfInput)

-- Using $> changes the types in ways that jml doesn't understand.
{-# ANN durationParser ("HLint: ignore Use $>" :: String) #-}
-- | Parser for 'Duration'.
durationParser :: Parser Duration
durationParser = signed (sum <$> some durationUnits)
  where
    durationUnits = mul <$> (multiplier <?> "multiplier") <*> (unit <?> "unit")
    multiplier = double
    unit =
      ("ns" *> pure nanosecond) <|>
      ("us" *> pure microsecond) <|>
      ("ms" *> pure millisecond) <|>
      ("s" *> pure second) <|>
      ("m" *> pure minute) <|>
      ("h" *> pure hour)

formatDuration :: Duration -> Text
formatDuration d@(Duration n)
  | d < Duration 0  = "-" <> formatDuration (abs d)
  | d == Duration 0 = "0s"
  | d < microsecond = fmtInt n <> "ns"
  | d < millisecond = fmtFrac (n % 1000) <> "us"
  | d < second = fmtFrac (n % 1000000) <> "ms"
  | otherwise =
      let (totalSeconds, nanoseconds) = n `divMod` 1000000000
          (totalMinutes, seconds) = totalSeconds `divMod` 60
          (hours, minutes) = totalMinutes `divMod` 60
      in (if hours > 0 then fmtInt hours <> "h" else "") <>
         (if totalMinutes > 0 then fmtInt minutes <> "m" else "") <>
         (if totalSeconds > 0
          then fmtFrac ((seconds % 1) + (nanoseconds % 1000000000)) <> "s"
          else panic ("Expected positive seconds but found " <> show totalSeconds))
  where
    fmtFrac x =
      let x' = truncate x
      in if x == fromInteger x' then fmtInt (fromInteger x')
         else show @Double @Text . fromRational . toRational $ x
    fmtInt = show
