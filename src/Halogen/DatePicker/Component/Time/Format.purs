module Halogen.Datapicker.Component.Time.Format
  ( Format
  , Command(..)
  , fromString
  , toDateTimeFormatter
  , unformat
  , format
  ) where

import Prelude
import Data.Foldable (class Foldable, foldMap)
import Data.String (joinWith)
import Data.Traversable (traverse)
import Data.List (List)
import Data.DateTime (DateTime(..), canonicalDate, time)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Formatter.DateTime as FDT
import Halogen.Datapicker.Constraint as Constraint
import Data.Time (Time)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

{--
Placeholder
--
YearFull
YearTwoDigits
YearAbsolute
MonthFull
MonthShort
MonthTwoDigits
DayOfMonthTwoDigits
DayOfMonth
DayOfWeek
--}
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
  let errs = Constraint.runConstraint timeFormatConstraint fmt
  when (errs /= []) $ Left $ joinWith "; " errs
  case traverse toTimeCommand fmt of
    Just fmt' -> pure $ Format fmt'
    Nothing -> Left "(unreachable) invalid FormatterCommand has leaked while checking constraints"
  where
  toTimeCommand :: FDT.FormatterCommand -> Maybe Command
  toTimeCommand FDT.Hours24 = Just Hours24
  toTimeCommand FDT.Hours12 = Just Hours12
  toTimeCommand FDT.Meridiem = Just Meridiem
  toTimeCommand FDT.MinutesTwoDigits = Just MinutesTwoDigits
  toTimeCommand FDT.Minutes = Just Minutes
  toTimeCommand FDT.SecondsTwoDigits = Just SecondsTwoDigits
  toTimeCommand FDT.Seconds = Just Seconds
  toTimeCommand FDT.Milliseconds = Just Milliseconds
  toTimeCommand FDT.MillisecondsTwoDigits = Just MillisecondsTwoDigits
  toTimeCommand FDT.MillisecondsShort = Just MillisecondsShort
  toTimeCommand (FDT.Placeholder str)= Just $ Placeholder str
  toTimeCommand _ = Nothing

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
unformat fmt dateStr = FDT.unformat (toDateTimeFormatter fmt) dateStr  <#> time

format ∷ Format -> Time -> String
format fmt = FDT.format (toDateTimeFormatter fmt) <<< dateTimeFromTime
  where
  dateTimeFromTime ∷ Time -> DateTime
  dateTimeFromTime = DateTime (canonicalDate bottom bottom bottom)


timeFormatConstraint :: ∀ g. Foldable g => Constraint.Constraint (g FDT.FormatterCommand)
timeFormatConstraint
  =   Constraint.notEmpty
  <> (Constraint.allowedValues allowedCommands)
  <> (Constraint.allowNoneOrOne [FDT.Milliseconds, FDT.MillisecondsTwoDigits, FDT.MillisecondsShort])
  <> (Constraint.allowNoneOrOne [FDT.SecondsTwoDigits, FDT.Seconds])
  <> (Constraint.allowNoneOrOne [FDT.MinutesTwoDigits, FDT.Minutes])
  <> (Constraint.allowNoneOrOne [FDT.Hours24, FDT.Hours12])
  <> (Constraint.allowNoneOrOne [FDT.Hours24, FDT.Meridiem])
  <> (Constraint.allowNoneOrAll [FDT.Hours12, FDT.Meridiem])
  where
  allowedCommands =
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
    -- TODO at this point the Constraint is working with Equality
    -- so we cant have FDT.Placeholder of anything here
    -- Constraint should be upadated to support that or ...
    , FDT.Placeholder ","
    , FDT.Placeholder " "
    , FDT.Placeholder "."
    , FDT.Placeholder "-"
    , FDT.Placeholder ":"
    ]
