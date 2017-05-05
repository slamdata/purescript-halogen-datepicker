module Halogen.Datapicker.Component.Date.Format
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
import Data.DateTime (DateTime(..), date)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Formatter.DateTime as FDT
import Halogen.Datapicker.Constraint as Constraint
import Data.Date (Date)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data Command
  = YearFull
  | YearTwoDigits
  | YearAbsolute
  | MonthFull
  | MonthShort
  | MonthTwoDigits
  | DayOfMonthTwoDigits
  | DayOfMonth
  | Placeholder String
  -- NOTE `DayOfWeek` value is not fully supported in ps-formatters itself
  -- as it it only has point to use with `week number`.
  -- once it's supported then we could use it here too.


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
  let errs = Constraint.runConstraint formatConstraint fmt
  when (errs /= []) $ Left $ joinWith "; " errs
  case traverse toDateCommand fmt of
    Just fmt' -> pure $ Format fmt'
    Nothing -> Left "(unreachable) invalid FormatterCommand has leaked while checking constraints"
  where
  toDateCommand :: FDT.FormatterCommand -> Maybe Command
  toDateCommand FDT.YearFull = Just YearFull
  toDateCommand FDT.YearTwoDigits = Just YearTwoDigits
  toDateCommand FDT.YearAbsolute = Just YearAbsolute
  toDateCommand FDT.MonthFull = Just MonthFull
  toDateCommand FDT.MonthShort = Just MonthShort
  toDateCommand FDT.MonthTwoDigits = Just MonthTwoDigits
  toDateCommand FDT.DayOfMonthTwoDigits = Just DayOfMonthTwoDigits
  toDateCommand FDT.DayOfMonth = Just DayOfMonth
  -- toDateCommand FDT.DayOfWeek = Just DayOfWeek
  toDateCommand (FDT.Placeholder str)= Just $ Placeholder str
  toDateCommand _ = Nothing

toDateTimeFormatter ∷ Format -> FDT.Formatter
toDateTimeFormatter (Format fmt) = foldMap (pure <<< toDTCommand) fmt
  where
  toDTCommand YearFull = FDT.YearFull
  toDTCommand YearTwoDigits = FDT.YearTwoDigits
  toDTCommand YearAbsolute = FDT.YearAbsolute
  toDTCommand MonthFull = FDT.MonthFull
  toDTCommand MonthShort = FDT.MonthShort
  toDTCommand MonthTwoDigits = FDT.MonthTwoDigits
  toDTCommand DayOfMonthTwoDigits = FDT.DayOfMonthTwoDigits
  toDTCommand DayOfMonth = FDT.DayOfMonth
  -- toDTCommand DayOfWeek = FDT.DayOfWeek
  toDTCommand (Placeholder str) = FDT.Placeholder str

unformat ∷ Format -> String -> Either String Date
unformat fmt str = FDT.unformat (toDateTimeFormatter fmt) str <#> date

format ∷ Format -> Date -> String
format fmt = FDT.format (toDateTimeFormatter fmt) <<< toDateTime
  where
  toDateTime ∷ Date -> DateTime
  toDateTime d = DateTime d bottom


formatConstraint :: ∀ g. Foldable g => Constraint.Constraint (g FDT.FormatterCommand)
formatConstraint
  =   Constraint.notEmpty
  <> (Constraint.allowedValues allowedCommands)
  <> (Constraint.allowNoneOrOne [FDT.YearFull, FDT.YearTwoDigits, FDT.YearAbsolute])
  <> (Constraint.allowNoneOrOne [FDT.MonthFull, FDT.MonthShort, FDT.MonthTwoDigits])
  <> (Constraint.allowNoneOrOne [FDT.DayOfMonthTwoDigits, FDT.DayOfMonth])
  where
  allowedCommands =
    [ FDT.YearFull
    , FDT.YearTwoDigits
    , FDT.YearAbsolute
    , FDT.MonthFull
    , FDT.MonthShort
    , FDT.MonthTwoDigits
    , FDT.DayOfMonthTwoDigits
    , FDT.DayOfMonth
    -- , FDT.DayOfWeek
    -- TODO at this point the Constraint is working with Equality
    -- so we cant have FDT.Placeholder of anything here
    -- Constraint should be upadated to support that or ...
    , FDT.Placeholder ","
    , FDT.Placeholder " "
    , FDT.Placeholder "."
    , FDT.Placeholder "-"
    , FDT.Placeholder ":"
    ]
