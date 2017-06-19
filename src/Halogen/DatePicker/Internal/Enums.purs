module Halogen.Datapicker.Internal.Enums where

import Data.Date
  ( Date
  , Year, Month, Day
  , year, month, day
  , exactDate
  )

import Data.Time
  ( Time
  , hour, millisecond
  , setHour, setMillisecond
  )
import Data.String as Str
import Data.Maybe (Maybe(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype, unwrap)
import Prelude
import Data.Enum
  ( class Enum
  , class BoundedEnum
  , Cardinality(..)
  , cardinality
  , toEnum
  , fromEnum
  , defaultSucc
  , defaultPred)



meridiem :: Time -> Meridiem
meridiem = hour >>> fromEnum >>> \h -> if h >= 12 then PM else AM

setMeridiem :: Meridiem -> Time -> Maybe Time
setMeridiem m t = newHour <#> (_ `setHour` t)
  where
  h = fromEnum (hour t)
  newHour = toEnum $ case m of
    AM -> if h > 12 then h - 12 else h
    PM -> if h < 12 then h + 12 else h

data Meridiem = AM | PM
derive instance meridiemEq ∷ Eq Meridiem
derive instance meridiemOrd ∷ Ord Meridiem
instance meridiemShow ∷ Show Meridiem where
  show AM = "AM"
  show PM = "PM"

instance meridiemBounded ∷ Bounded Meridiem where
  bottom = AM
  top = PM

instance meridiemEnum ∷ Enum Meridiem where
  pred PM = Just AM
  pred _ = Nothing
  succ AM = Just PM
  succ _ = Nothing

instance meridiemBoundedEnum ∷ BoundedEnum Meridiem where
  cardinality = Cardinality 2
  toEnum 1 = Just AM
  toEnum 2 = Just PM
  toEnum _ = Nothing
  fromEnum AM = 1
  fromEnum PM = 2


hour12 :: Time -> Hour12
hour12 = hour >>> fromEnum >>> (\h -> if h >= 12 then h - 12 else h) >>> Hour12

setHour12 :: Hour12 -> Time -> Maybe Time
setHour12 (Hour12 h) t = toEnum (if (fromEnum $ hour t) < 12 then h else h + 12) <#> (_ `setHour` t)


newtype Hour12 = Hour12 Int

derive instance hour12Newtype :: Newtype Hour12 _
derive instance hour12Generic :: Generic Hour12 _
derive instance hour12Eq :: Eq Hour12
derive instance hour12Ord :: Ord Hour12

instance hour12Bounded :: Bounded Hour12 where
  bottom = Hour12 0
  top = Hour12 11

instance hour12Enum :: Enum Hour12 where
  succ = defaultSucc toEnum fromEnum
  pred = defaultPred toEnum fromEnum

instance hour12BoundedEnum :: BoundedEnum Hour12 where
  cardinality = Cardinality 12
  toEnum n | n >= 0 && n <= 11 = Just $ Hour12 n
  toEnum _ = Nothing
  fromEnum = unwrap

instance hour12Show :: Show Hour12 where
  show = genericShow

millisecond2 :: Time -> Millisecond2
millisecond2 = millisecond >>> fromEnum >>> (_ / 10) >>> Millisecond2

setMillisecond2 :: Millisecond2 -> Time -> Maybe Time
setMillisecond2 (Millisecond2 ms) t = toEnum (ms * 10) <#> (_ `setMillisecond` t)


newtype Millisecond2 = Millisecond2 Int
derive instance millisecond2Newtype :: Newtype Millisecond2 _
derive instance millisecond2Generic :: Generic Millisecond2 _
derive instance millisecond2Eq :: Eq Millisecond2
derive instance millisecond2Ord :: Ord Millisecond2

instance millisecond2Bounded :: Bounded Millisecond2 where
  bottom = Millisecond2 0
  top = Millisecond2 99

instance millisecond2Enum :: Enum Millisecond2 where
  succ = defaultSucc toEnum fromEnum
  pred = defaultPred toEnum fromEnum

instance millisecond2BoundedEnum :: BoundedEnum Millisecond2 where
  cardinality = Cardinality 100
  toEnum n | n >= 0 && n <= 99 = Just $ Millisecond2 n
  toEnum _ = Nothing
  fromEnum = unwrap

instance millisecond2Show :: Show Millisecond2 where
  show = genericShow


millisecond1 :: Time -> Millisecond1
millisecond1 = millisecond >>> fromEnum >>> (_ / 100) >>> Millisecond1

setMillisecond1 :: Millisecond1 -> Time -> Maybe Time
setMillisecond1 (Millisecond1 ms) t = toEnum (ms * 100) <#> (_ `setMillisecond` t)


newtype Millisecond1 = Millisecond1 Int
derive instance millisecond1Newtype :: Newtype Millisecond1 _
derive instance millisecond1Generic :: Generic Millisecond1 _
derive instance millisecond1Eq :: Eq Millisecond1
derive instance millisecond1Ord :: Ord Millisecond1

instance millisecond1Bounded :: Bounded Millisecond1 where
  bottom = Millisecond1 0
  top = Millisecond1 9

instance millisecond1Enum :: Enum Millisecond1 where
  succ = defaultSucc toEnum fromEnum
  pred = defaultPred toEnum fromEnum

instance millisecond1BoundedEnum :: BoundedEnum Millisecond1 where
  cardinality = Cardinality 10
  toEnum n | n >= 0 && n <= 9 = Just $ Millisecond1 n
  toEnum _ = Nothing
  fromEnum = unwrap

instance millisecond1Show :: Show Millisecond1 where
  show = genericShow




monthShort :: Date -> MonthShort
monthShort = month >>> MonthShort


newtype MonthShort = MonthShort Month
derive instance monthShortNewtype :: Newtype MonthShort _
derive instance monthShortGeneric :: Generic MonthShort _
derive instance monthShortEq :: Eq MonthShort
derive instance monthShortOrd :: Ord MonthShort

instance monthShortBounded :: Bounded MonthShort where
  bottom = MonthShort bottom
  top = MonthShort top

instance monthShortEnum :: Enum MonthShort where
  succ = defaultSucc toEnum fromEnum
  pred = defaultPred toEnum fromEnum

instance monthShortBoundedEnum :: BoundedEnum MonthShort where
  cardinality = Cardinality $ unwrap (cardinality :: Cardinality Month)
  toEnum n = toEnum n <#> MonthShort
  fromEnum (MonthShort m) = fromEnum m

instance monthShortShow :: Show MonthShort where
  show (MonthShort m) = Str.take 3 $ show m

year2 :: Date -> Year2
year2 = year >>> fromEnum >>> \y -> Year2 $ y - (y `unPrecise` 100)

setYear2 :: Year2 -> Date -> Maybe Date
setYear2 (Year2 n) d = (toEnum $ ((fromEnum $ year d) `unPrecise` 100) + n) >>= (_ `setYear` d)

newtype Year2 = Year2 Int
derive instance year2Newtype :: Newtype Year2 _
derive instance year2Generic :: Generic Year2 _
derive instance year2Eq :: Eq Year2
derive instance year2Ord :: Ord Year2

instance year2Bounded :: Bounded Year2 where
  bottom = Year2 0
  top = Year2 99

instance year2Enum :: Enum Year2 where
  succ = defaultSucc toEnum fromEnum
  pred = defaultPred toEnum fromEnum

instance year2BoundedEnum :: BoundedEnum Year2 where
  cardinality = Cardinality 100
  toEnum n | n >= 0 && n <= 99 = Just $ Year2 n
  toEnum _ = Nothing
  fromEnum = unwrap

instance year2Show :: Show Year2 where
  show = genericShow


year4 :: Date -> Year4
year4 = year >>> fromEnum >>> \y -> Year4 $ y - (y `unPrecise` 10000)

setYear4 :: Year4 -> Date -> Maybe Date
setYear4 (Year4 n) d = toEnum n >>= (_ `setYear` d)


newtype Year4 = Year4 Int
derive instance year4Newtype :: Newtype Year4 _
derive instance year4Generic :: Generic Year4 _
derive instance year4Eq :: Eq Year4
derive instance year4Ord :: Ord Year4

instance year4Bounded :: Bounded Year4 where
  bottom = Year4 0
  top = Year4 9999

instance year4Enum :: Enum Year4 where
  succ = defaultSucc toEnum fromEnum
  pred = defaultPred toEnum fromEnum

instance year4BoundedEnum :: BoundedEnum Year4 where
  cardinality = Cardinality 10000
  toEnum n | n >= 0 && n <= 9999 = Just $ Year4 n
  toEnum _ = Nothing
  fromEnum = unwrap

instance year4Show :: Show Year4 where
  show = genericShow


setYear :: Year -> Date -> Maybe Date
setYear a d = exactDate a (month d) (day d)

setMonth :: Month -> Date -> Maybe Date
setMonth a d = exactDate (year d) a (day d)

setDay :: Day -> Date -> Maybe Date
setDay a d = exactDate (year d) (month d) a

-- > 123456789 `unPrecise` 1000
-- 123456000
unPrecise :: Int -> Int -> Int
unPrecise n by = n / by * by
