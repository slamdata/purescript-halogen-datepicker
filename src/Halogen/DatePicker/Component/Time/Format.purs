module Halogen.Datapicker.Component.Time.Format where

import Prelude
import Data.Foldable (foldMap)
import Data.List (List)
import Data.Formatter.DateTime as FDT
import Data.Tuple (Tuple(..))
import Data.Time
  ( Time
  , second
  , minute
  , hour
  , millisecond
  )
import Data.Enum (fromEnum)

data Command = Hour | Minute | Second | Millisecond
-- derive instance commandGeneric :: Generic Command _
derive instance commandEq :: Eq Command
derive instance commandOrd :: Ord Command


type Format = List Command

type CommandProp =
  { placeholder ∷ String
  , range ∷ Tuple Int Int
  }

getProps ∷ Command -> CommandProp
getProps Hour = { placeholder: "Hour" , range: Tuple 0 23 }
getProps Minute = { placeholder: "Minute" , range: Tuple 0 59 }
getProps Second = { placeholder: "Second" , range: Tuple 0 59 }
getProps Millisecond = { placeholder: "Millisecond" , range: Tuple 0 999 }

toGetter ∷ Command -> Time -> Int
toGetter Second = second >>> fromEnum
toGetter Minute = minute >>> fromEnum
toGetter Hour = hour >>> fromEnum
toGetter Millisecond = millisecond >>> fromEnum

toDateTimeFormatter ∷ Format -> FDT.Formatter
toDateTimeFormatter cs = foldMap (pure <<< f) cs
  where
  f Hour = FDT.Hours24
  f Minute = FDT.MinutesTwoDigits
  f Second = FDT.SecondsTwoDigits
  f Millisecond = FDT.Milliseconds
