module Halogen.Datapicker.Component.Date.Format
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
import Data.DateTime (DateTime(..), date)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Formatter.DateTime as FDT
import Halogen.Datapicker.Component.Internal.Constraint as C
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
  let errs = C.runConstraint formatConstraint fmt
  when (errs /= []) $ Left $ joinWith "; " errs
  case traverse toCommand fmt of
    Just fmt' -> pure $ Format fmt'
    Nothing -> Left "(unreachable) invalid FormatterCommand has leaked while checking constraints"

toCommand :: FDT.FormatterCommand -> Maybe Command
toCommand FDT.YearFull = Just YearFull
toCommand FDT.YearTwoDigits = Just YearTwoDigits
toCommand FDT.YearAbsolute = Just YearAbsolute
toCommand FDT.MonthFull = Just MonthFull
toCommand FDT.MonthShort = Just MonthShort
toCommand FDT.MonthTwoDigits = Just MonthTwoDigits
toCommand FDT.DayOfMonthTwoDigits = Just DayOfMonthTwoDigits
toCommand FDT.DayOfMonth = Just DayOfMonth
-- toCommand FDT.DayOfWeek = Just DayOfWeek
toCommand (FDT.Placeholder str)= Just $ Placeholder str
toCommand _ = Nothing

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


formatConstraint :: ∀ g. Foldable g => C.Constraint (g FDT.FormatterCommand)
formatConstraint
  =  C.notEmpty
  <> C.allowedValues FDT.printFormatterCommand allowedCommands
  <> C.allowNoneOrOne (C.reShow FDT.printFormatterCommand <$> [FDT.YearFull, FDT.YearTwoDigits, FDT.YearAbsolute])
  <> C.allowNoneOrOne (C.reShow FDT.printFormatterCommand <$> [FDT.MonthFull, FDT.MonthShort, FDT.MonthTwoDigits])
  <> C.allowNoneOrOne (C.reShow FDT.printFormatterCommand <$> [FDT.DayOfMonthTwoDigits, FDT.DayOfMonth])
  where
  allowedCommands = (C.reShow FDT.printFormatterCommand <$>
    [ FDT.YearFull
    , FDT.YearTwoDigits
    , FDT.YearAbsolute
    , FDT.MonthFull
    , FDT.MonthShort
    , FDT.MonthTwoDigits
    , FDT.DayOfMonthTwoDigits
    , FDT.DayOfMonth
    ]) <>
    [ C.EqPred
        "'Placeholder'"
        case _ of
          FDT.Placeholder _ -> true
          _ -> false
    ]
