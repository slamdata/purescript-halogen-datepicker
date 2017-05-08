module Halogen.Datapicker.Component.Time.Format
  ( Format
  , Command(..)
  , fromString
  , fromDateTimeFormatter
  , toDateTimeFormatter
  , toCommand
  , unformat
  , format
  ) where

import Prelude
import Data.Foldable (class Foldable, foldMap)
import Data.String (joinWith)
import Data.Traversable (traverse)
import Data.List (List)
import Data.DateTime (DateTime(..), time)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Formatter.DateTime as FDT
import Halogen.Datapicker.Component.Internal.Constraint as C
import Data.Time (Time)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data Command
  = Hours24
  | Hours12
  | Meridiem
  | MinutesTwoDigits
  | Minutes
  | SecondsTwoDigits
  | Seconds
  | Milliseconds
  | MillisecondsTwoDigits
  | MillisecondsShort
  | Placeholder String


derive instance commandGeneric :: Generic Command _
derive instance commandEq :: Eq Command
derive instance commandOrd :: Ord Command
instance commandShow :: Show Command where
  show = genericShow


newtype Format = Format (List Command)
derive instance formatNewtype :: Newtype Format _
derive instance formatGeneric :: Generic Format _
instance formatShow :: Show Format where
  show = genericShow
derive instance formatEq :: Eq Format
derive instance formatOrd :: Ord Format

fromString :: String -> Either String Format
fromString s = FDT.parseFormatString s >>= fromDateTimeFormatter

fromDateTimeFormatter :: FDT.Formatter -> Either String Format
fromDateTimeFormatter fmt = do
  let errs = C.runConstraint formatConstraint fmt
  when (errs /= []) $ Left $ joinWith "; " errs
  case traverse toCommand fmt of
    Just fmt' -> pure $ Format fmt'
    Nothing -> Left "(unreachable) invalid FormatterCommand has leaked while checking constraints"

toCommand :: FDT.FormatterCommand -> Maybe Command
toCommand FDT.Hours24 = Just Hours24
toCommand FDT.Hours12 = Just Hours12
toCommand FDT.Meridiem = Just Meridiem
toCommand FDT.MinutesTwoDigits = Just MinutesTwoDigits
toCommand FDT.Minutes = Just Minutes
toCommand FDT.SecondsTwoDigits = Just SecondsTwoDigits
toCommand FDT.Seconds = Just Seconds
toCommand FDT.Milliseconds = Just Milliseconds
toCommand FDT.MillisecondsTwoDigits = Just MillisecondsTwoDigits
toCommand FDT.MillisecondsShort = Just MillisecondsShort
toCommand (FDT.Placeholder str)= Just $ Placeholder str
toCommand _ = Nothing

toDateTimeFormatter ∷ Format -> FDT.Formatter
toDateTimeFormatter (Format fmt) = foldMap (pure <<< toDTCommand) fmt
  where
  toDTCommand Hours24 = FDT.Hours24
  toDTCommand Hours12 = FDT.Hours12
  toDTCommand Meridiem = FDT.Meridiem
  toDTCommand MinutesTwoDigits = FDT.MinutesTwoDigits
  toDTCommand Minutes = FDT.Minutes
  toDTCommand SecondsTwoDigits = FDT.SecondsTwoDigits
  toDTCommand Seconds = FDT.Seconds
  toDTCommand Milliseconds = FDT.Milliseconds
  toDTCommand MillisecondsTwoDigits = FDT.MillisecondsTwoDigits
  toDTCommand MillisecondsShort = FDT.MillisecondsShort
  toDTCommand (Placeholder str) = FDT.Placeholder str

unformat ∷ Format -> String -> Either String Time
unformat fmt str = FDT.unformat (toDateTimeFormatter fmt) str  <#> time

format ∷ Format -> Time -> String
format fmt = FDT.format (toDateTimeFormatter fmt) <<< toDateTime
  where
  toDateTime ∷ Time -> DateTime
  toDateTime = DateTime bottom


formatConstraint :: ∀ g. Foldable g => C.Constraint (g FDT.FormatterCommand)
formatConstraint
  =  C.notEmpty
  <> C.allowedValues FDT.printFormatterCommand allowedCommands
  <> C.allowNoneOrOne (C.reShow FDT.printFormatterCommand <$> [FDT.Milliseconds, FDT.MillisecondsTwoDigits, FDT.MillisecondsShort])
  <> C.allowNoneOrOne (C.reShow FDT.printFormatterCommand <$> [FDT.SecondsTwoDigits, FDT.Seconds])
  <> C.allowNoneOrOne (C.reShow FDT.printFormatterCommand <$> [FDT.MinutesTwoDigits, FDT.Minutes])
  <> C.allowNoneOrOne (C.reShow FDT.printFormatterCommand <$> [FDT.Hours24, FDT.Hours12])
  <> C.allowNoneOrOne (C.reShow FDT.printFormatterCommand <$> [FDT.Hours24, FDT.Meridiem])
  <> C.allowNoneOrAll (C.reShow FDT.printFormatterCommand <$> [FDT.Hours12, FDT.Meridiem])
  where
  allowedCommands = (C.reShow FDT.printFormatterCommand <$>
    [ FDT.Hours24
    , FDT.Hours12
    , FDT.Meridiem
    , FDT.MinutesTwoDigits
    , FDT.Minutes
    , FDT.SecondsTwoDigits
    , FDT.Seconds
    , FDT.Milliseconds
    , FDT.MillisecondsTwoDigits
    , FDT.MillisecondsShort
    ]) <>
    [ C.EqPred
        "'Placeholder'"
        case _ of
          FDT.Placeholder _ -> true
          _ -> false
    ]
