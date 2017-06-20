module Halogen.Datapicker.Format.Time
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

import Data.Array (fromFoldable)
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
import Halogen.Datapicker.Internal.Constraint as C
import Halogen.Datapicker.Internal.Enums (hour12, meridiem, millisecond1, millisecond2, setHour12, setMeridiem, setMillisecond1, setMillisecond2)

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


toSetter ∷ Command -> Int -> Time -> Maybe Time
toSetter Hours24 n t = toEnum n <#> ( _ `setHour` t)
toSetter Hours12 n t = toEnum n >>= (_ `setHour12` t)
toSetter Meridiem n t = toEnum n >>= (_ `setMeridiem` t)
toSetter MinutesTwoDigits n t = toEnum n <#> ( _ `setMinute` t)
toSetter Minutes n t = toEnum n <#> ( _ `setMinute` t)
toSetter SecondsTwoDigits n t = toEnum n <#> ( _ `setSecond` t)
toSetter Seconds n t = toEnum n <#> ( _ `setSecond` t)
toSetter Milliseconds n t =toEnum n <#>  (_ `setMillisecond` t)
toSetter MillisecondsTwoDigits n t = toEnum n >>= (_ `setMillisecond2` t)
toSetter MillisecondsShort n t = toEnum n >>= (_ `setMillisecond1` t)
toSetter (Placeholder _) _ t = pure t

toGetter ∷ Command -> Time -> Maybe Int
toGetter Hours24 t = Just $ fromEnum $ hour t
toGetter Hours12 t = Just $ fromEnum $ hour12 t
toGetter Meridiem t = Just $ fromEnum $ meridiem t
toGetter MinutesTwoDigits t = Just $ fromEnum $ minute t
toGetter Minutes t = Just $ fromEnum $ minute t
toGetter SecondsTwoDigits t = Just $ fromEnum $ second t
toGetter Seconds t = Just $ fromEnum $ second t
toGetter Milliseconds t = Just $ fromEnum $ millisecond t
toGetter MillisecondsTwoDigits t = Just $ fromEnum $ millisecond2 t
toGetter MillisecondsShort t = Just $ fromEnum $ millisecond1 t
toGetter (Placeholder _) t = Nothing


fromString ∷ String -> Either String Format
fromString s = FDT.parseFormatString s >>= fromDateTimeFormatter

fromDateTimeFormatter ∷ FDT.Formatter -> Either String Format
fromDateTimeFormatter fmt = do
  let errs = C.runConstraint formatConstraint fmt
  when (errs /= []) $ Left $ joinWith "; " errs
  case traverse toCommand fmt of
    Just fmt' -> pure $ Format $ fromFoldable fmt'
    Nothing -> Left "(unreachable) invalid FormatterCommand has leaked while checking constraints"

toCommand ∷ FDT.FormatterCommand -> Maybe Command
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


formatConstraint ∷ ∀ g. Foldable g => C.Constraint (g FDT.FormatterCommand)
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
