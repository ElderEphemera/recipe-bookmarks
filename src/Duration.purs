module Duration where

import Prelude

import Data.Argonaut
  ( class DecodeJson, class EncodeJson, JsonDecodeError(..)
  , decodeJson, encodeJson
  )
import Data.Array (foldMap, intercalate)
import Data.Array.NonEmpty (tail)
import Data.Either (hush, note)
import Data.Int (fromNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.String.Regex (match, regex)


data Duration = Duration
  { years :: Number
  , months :: Number
  , weeks :: Number
  , days :: Number
  , hours :: Number
  , minutes :: Number
  , seconds :: Number
  }


instance decodeDuration :: DecodeJson Duration where
  decodeJson json = note MissingValue <<< parseDuration =<< decodeJson json

parseDuration :: String -> Maybe Duration
parseDuration str = do
  pat <- hush $ regex durationRegex mempty
  matches <- match pat str
  case maybe 0.0 parseFloat <$> tail matches of
    [years, months, weeks, days, hours, minutes, seconds] -> Just $ Duration
      {years, months, weeks, days, hours, minutes, seconds}
    _ -> Nothing

durationRegex :: String
durationRegex
  =  "P"  <> foldMap component ["Y","M","W","D"]
  <> "T?" <> foldMap component ["H","M","S"]
  where component tag = "(?:([\\d\\.]+)" <> tag <> ")?"

foreign import parseFloat :: String -> Number


instance encodeDuration :: EncodeJson Duration where
  encodeJson (Duration d) = encodeJson
    $  "P"
    <> component "Y" d.years
    <> component "M" d.months
    <> component "W" d.weeks
    <> component "D" d.days
    <> "T"
    <> component "H" d.hours
    <> component "M" d.minutes
    <> component "S" d.seconds
    where component tag val = if val == 0.0 then "" else show val <> tag


showDuration :: Duration -> String
showDuration (Duration d) = intercalate " "
  [ component d.years "Year"
  , component d.weeks "Week"
  , component d.days "Day"
  , component d.hours "Hour"
  , component d.minutes "Minute"
  , component d.seconds "Second"
  ] where
    component value word = case fromNumber value of
      Just 0 -> ""
      Just 1 -> "1 " <> word
      Just n -> show n <> " " <> word <> "s"
      Nothing -> show value <> " " <> word <> "s"
