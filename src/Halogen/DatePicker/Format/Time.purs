module Halogen.Datepicker.Format.Time
  ( Format
  , Command(..)
  , fromString
  , fromDateTimeFormatter
  , toDateTimeFormatter
  , toCommand
  , unformat
  , format
  , toSetter
  , toGetter
  ) where

import Prelude

import Data.Array (fromFoldable, null)
import Data.DateTime (DateTime(..), time)
import Data.Either (Either(..))
import Data.Enum (fromEnum, toEnum)
import Data.Foldable (class Foldable, foldMap)
import Data.Formatter.DateTime as FDT
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String (joinWith)
import Data.Time (Time, hour, millisecond, minute, second, setHour, setMillisecond, setMinute, setSecond)
import Data.Traversable (traverse)
import Halogen.Datepicker.Internal.Constraint as C
import Halogen.Datepicker.Internal.Enums (hour12, meridiem, millisecond1, millisecond2, setHour12, setMeridiem, setMillisecond1, setMillisecond2)

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


derive instance commandGeneric ∷ Generic Command _
derive instance commandEq ∷ Eq Command
derive instance commandOrd ∷ Ord Command
instance commandShow ∷ Show Command where
  show = genericShow


newtype Format = Format (Array Command)
derive instance formatNewtype ∷ Newtype Format _
derive instance formatGeneric ∷ Generic Format _
instance formatShow ∷ Show Format where
  show = genericShow
derive instance formatEq ∷ Eq Format
derive instance formatOrd ∷ Ord Format


toSetter ∷ Command → Int → Time → Maybe Time
toSetter cmd n t = case cmd of
  Hours24 → toEnum n <#> ( _ `setHour` t)
  Hours12 → toEnum n >>= (_ `setHour12` t)
  Meridiem → toEnum n >>= (_ `setMeridiem` t)
  MinutesTwoDigits → toEnum n <#> ( _ `setMinute` t)
  Minutes → toEnum n <#> ( _ `setMinute` t)
  SecondsTwoDigits → toEnum n <#> ( _ `setSecond` t)
  Seconds → toEnum n <#> ( _ `setSecond` t)
  Milliseconds →toEnum n <#>  (_ `setMillisecond` t)
  MillisecondsTwoDigits → toEnum n >>= (_ `setMillisecond2` t)
  MillisecondsShort → toEnum n >>= (_ `setMillisecond1` t)
  Placeholder _ → pure t

toGetter ∷ Command → Time → Maybe Int
toGetter cmd t = case cmd of
  Hours24 → Just $ fromEnum $ hour t
  Hours12 → Just $ fromEnum $ hour12 t
  Meridiem → Just $ fromEnum $ meridiem t
  MinutesTwoDigits → Just $ fromEnum $ minute t
  Minutes → Just $ fromEnum $ minute t
  SecondsTwoDigits → Just $ fromEnum $ second t
  Seconds → Just $ fromEnum $ second t
  Milliseconds → Just $ fromEnum $ millisecond t
  MillisecondsTwoDigits → Just $ fromEnum $ millisecond2 t
  MillisecondsShort → Just $ fromEnum $ millisecond1 t
  Placeholder _ → Nothing


fromString ∷ String → Either String Format
fromString s = FDT.parseFormatString s >>= fromDateTimeFormatter

fromDateTimeFormatter ∷ FDT.Formatter → Either String Format
fromDateTimeFormatter fmt = do
  let errs = C.runConstraint formatConstraint fmt
  unless (null errs) $ Left $ joinWith "; " errs
  case traverse toCommand fmt of
    Just fmt' → pure $ Format $ fromFoldable fmt'
    Nothing → Left "(unreachable) invalid FormatterCommand has leaked while checking constraints"

toCommand ∷ FDT.FormatterCommand → Maybe Command
toCommand = case _ of
  FDT.Hours24 → Just Hours24
  FDT.Hours12 → Just Hours12
  FDT.Meridiem → Just Meridiem
  FDT.MinutesTwoDigits → Just MinutesTwoDigits
  FDT.Minutes → Just Minutes
  FDT.SecondsTwoDigits → Just SecondsTwoDigits
  FDT.Seconds → Just Seconds
  FDT.Milliseconds → Just Milliseconds
  FDT.MillisecondsTwoDigits → Just MillisecondsTwoDigits
  FDT.MillisecondsShort → Just MillisecondsShort
  FDT.Placeholder str → Just $ Placeholder str
  _ → Nothing

toDateTimeFormatter ∷ Format → FDT.Formatter
toDateTimeFormatter (Format fmt) = foldMap (pure <<< toDTCommand) fmt

toDTCommand ∷ Command → FDT.FormatterCommand
toDTCommand = case _ of
  Hours24 → FDT.Hours24
  Hours12 → FDT.Hours12
  Meridiem → FDT.Meridiem
  MinutesTwoDigits → FDT.MinutesTwoDigits
  Minutes → FDT.Minutes
  SecondsTwoDigits → FDT.SecondsTwoDigits
  Seconds → FDT.Seconds
  Milliseconds → FDT.Milliseconds
  MillisecondsTwoDigits → FDT.MillisecondsTwoDigits
  MillisecondsShort → FDT.MillisecondsShort
  Placeholder str → FDT.Placeholder str

unformat ∷ Format → String → Either String Time
unformat fmt str = FDT.unformat (toDateTimeFormatter fmt) str  <#> time

format ∷ Format → Time → String
format fmt = FDT.format (toDateTimeFormatter fmt) <<< toDateTime
  where
  toDateTime ∷ Time → DateTime
  toDateTime = DateTime bottom


formatConstraint ∷ ∀ g. Foldable g ⇒ C.Constraint (g FDT.FormatterCommand)
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
          FDT.Placeholder _ → true
          _ → false
    ]
