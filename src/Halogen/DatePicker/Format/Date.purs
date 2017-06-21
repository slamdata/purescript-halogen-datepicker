module Halogen.Datepicker.Format.Date
  ( Format
  , Command(..)
  , toSetter
  , toGetter
  , fromString
  , fromDateTimeFormatter
  , toDateTimeFormatter
  , toCommand
  , unformat
  , format
  ) where

import Prelude

import Data.Array (fromFoldable)
import Data.Date (Date, day, month, year)
import Data.DateTime (DateTime(..), date)
import Data.Either (Either(..))
import Data.Enum (fromEnum, toEnum)
import Data.Foldable (class Foldable, foldMap)
import Data.Formatter.DateTime as FDT
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String (joinWith)
import Data.Traversable (traverse)
import Halogen.Datepicker.Internal.Constraint as C
import Halogen.Datepicker.Internal.Enums (monthShort, setDay, setMonth, setYear, setYear2, setYear4, year2, year4)

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
  -- as it it only has point to use when we have `week number` in date format.


derive instance commandGeneric ∷ Generic Command _
derive instance commandEq ∷ Eq Command
derive instance commandOrd ∷ Ord Command
instance commandShow ∷ Show Command where
  show = genericShow

toSetter ∷ Command → Int → Date → Maybe Date
toSetter cmd n d = case cmd of
  YearFull → toEnum n >>= (_ `setYear4` d)
  YearTwoDigits → toEnum n >>= (_ `setYear2` d)
  YearAbsolute → toEnum n >>= (_ `setYear` d)
  MonthFull → toEnum n >>= (_ `setMonth` d)
  MonthShort → toEnum n >>= (_ `setMonth` d)
  MonthTwoDigits → toEnum n >>= (_ `setMonth` d)
  DayOfMonthTwoDigits → toEnum n >>= (_ `setDay` d)
  DayOfMonth → toEnum n >>= (_ `setDay` d)
  Placeholder _ → pure d


toGetter ∷ Command → Date → Maybe Int
toGetter cmd d = case cmd of
  YearFull → Just $ fromEnum $ year4 d
  YearTwoDigits → Just $ fromEnum $ year2 d
  YearAbsolute → Just $ fromEnum $ year d
  MonthFull → Just $ fromEnum $ month d
  MonthShort → Just $ fromEnum $ monthShort d
  MonthTwoDigits → Just $ fromEnum $ month d
  DayOfMonthTwoDigits → Just $ fromEnum $ day d
  DayOfMonth → Just $ fromEnum $ day d
  Placeholder str → Nothing


newtype Format = Format (Array Command)
derive instance formatNewtype ∷ Newtype Format _
derive instance formatGeneric ∷ Generic Format _
instance formatShow ∷ Show Format where
  show = genericShow
derive instance formatEq ∷ Eq Format
derive instance formatOrd ∷ Ord Format

fromString ∷ String → Either String Format
fromString s = FDT.parseFormatString s >>= fromDateTimeFormatter

fromDateTimeFormatter ∷ FDT.Formatter → Either String Format
fromDateTimeFormatter fmt = do
  let errs = C.runConstraint formatConstraint fmt
  when (errs /= []) $ Left $ joinWith "; " errs
  case traverse toCommand fmt of
    Just fmt' → pure $ Format $ fromFoldable fmt'
    Nothing → Left "(unreachable) invalid FormatterCommand has leaked while checking constraints"

toCommand ∷ FDT.FormatterCommand → Maybe Command
toCommand = case _ of
  FDT.YearFull → Just YearFull
  FDT.YearTwoDigits → Just YearTwoDigits
  FDT.YearAbsolute → Just YearAbsolute
  FDT.MonthFull → Just MonthFull
  FDT.MonthShort → Just MonthShort
  FDT.MonthTwoDigits → Just MonthTwoDigits
  FDT.DayOfMonthTwoDigits → Just DayOfMonthTwoDigits
  FDT.DayOfMonth → Just DayOfMonth
  FDT.Placeholder str → Just $ Placeholder str
  _ → Nothing

toDateTimeFormatter ∷ Format → FDT.Formatter
toDateTimeFormatter (Format fmt) = foldMap (pure <<< toDTCommand) fmt

toDTCommand ∷ Command → FDT.FormatterCommand
toDTCommand = case _ of
  YearFull →  FDT.YearFull
  YearTwoDigits →  FDT.YearTwoDigits
  YearAbsolute →  FDT.YearAbsolute
  MonthFull →  FDT.MonthFull
  MonthShort →  FDT.MonthShort
  MonthTwoDigits →  FDT.MonthTwoDigits
  DayOfMonthTwoDigits →  FDT.DayOfMonthTwoDigits
  DayOfMonth →  FDT.DayOfMonth
  Placeholder str →  FDT.Placeholder str

unformat ∷ Format → String → Either String Date
unformat fmt str = FDT.unformat (toDateTimeFormatter fmt) str <#> date

format ∷ Format → Date → String
format fmt = FDT.format (toDateTimeFormatter fmt) <<< toDateTime
  where
  toDateTime ∷ Date → DateTime
  toDateTime d = DateTime d bottom


formatConstraint ∷ ∀ g. Foldable g ⇒ C.Constraint (g FDT.FormatterCommand)
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
          FDT.Placeholder _ → true
          _ → false
    ]
