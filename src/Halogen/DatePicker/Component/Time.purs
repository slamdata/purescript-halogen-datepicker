module Halogen.Datapicker.Component.Time where

import Prelude
import Debug.Trace as D
import Halogen.Datapicker.Component.Internal.Enums
  ( hour12
  , meridiem
  , millisecond2
  , millisecond1
  , setHour12
  , setMeridiem
  , setMillisecond2
  , setMillisecond1
  )

import Halogen.Datapicker.Component.Elements (textElement, enumNumberElement, numberElement, choiseElement)
import Halogen.Datapicker.Component.Types (PickerQuery(..), PickerMessage(..))
import Data.Time
  ( Time, Hour, Millisecond
  , second, minute, hour, millisecond
  , setSecond, setMinute, setHour, setMillisecond
  )
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype, unwrap)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), toEnum, fromEnum, defaultSucc, defaultPred)
import Data.Functor.Coproduct (Coproduct, coproduct, right)
import Halogen.Datapicker.Component.Time.Format as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.Int as Int

data TimeQuery a = UpdateCommand F.Command String a

type Query = Coproduct (PickerQuery Time) TimeQuery
type Message = PickerMessage Time
type State =
  { format :: F.Format
  , time :: Time
  }

type DSL = H.ComponentDSL State Query Message
type HTML = H.ComponentHTML TimeQuery

initialStateFromFormat ∷ F.Format -> State
initialStateFromFormat format = {format: format, time: bottom}

picker ∷ ∀ m. F.Format -> H.Component HH.HTML Query Unit Message m
picker fmt = H.component
  { initialState: const $ initialStateFromFormat fmt
  , render: render <#> (map right)
  , eval: coproduct evalPicker evalTime
  , receiver: const Nothing
  }
  where
  render ∷ State -> HTML
  render {time, format} = HH.ul_ $ foldMap (pure <<< f) (unwrap format)
    where
    f cmd = HH.li_ [renderCommand time cmd]

-- NOTE
-- maybe we can splitt `___Element cmd {___} (____ t)`
-- in two patrs two pats:
-- 1) takes `Command` and returns coresponding function waiting for selected value (Command -> x -> HTML)
-- 2) takes `Command` and `Time` and returns value which must be selected (Command -> Time -> x)
-- and here both of them are combined.
-- we might need to remove replace `BoundedEnum a` with `Int` in `choiseElement`

renderCommand :: Time -> F.Command -> HTML
renderCommand t cmd@F.Hours24               = enumNumberElement (UpdateCommand cmd) { title: "Hours"} (hour t)
renderCommand t cmd@F.Hours12               = enumNumberElement (UpdateCommand cmd) { title: "Hours"} (hour12 t)
renderCommand t cmd@F.Meridiem              = choiseElement (UpdateCommand cmd) { title: "Meridiem" } (meridiem t)
renderCommand t cmd@F.MinutesTwoDigits      = enumNumberElement (UpdateCommand cmd) { title: "Minutes"} (minute t)
renderCommand t cmd@F.Minutes               = enumNumberElement (UpdateCommand cmd) { title: "Minutes"} (minute t)
renderCommand t cmd@F.SecondsTwoDigits      = enumNumberElement (UpdateCommand cmd) { title: "Seconds"} (second t)
renderCommand t cmd@F.Seconds               = enumNumberElement (UpdateCommand cmd) { title: "Seconds"} (second t)
renderCommand t cmd@F.Milliseconds          = enumNumberElement (UpdateCommand cmd) { title: "Milliseconds"} (millisecond t)
renderCommand t cmd@F.MillisecondsTwoDigits = enumNumberElement (UpdateCommand cmd) { title: "Milliseconds"} (millisecond2 t)
renderCommand t cmd@F.MillisecondsShort     = enumNumberElement (UpdateCommand cmd) { title: "Milliseconds"} (millisecond1 t)
renderCommand _ (F.Placeholder str)         = textElement { text: str}


-- TODO switch to Validation/Either instead of Maybe to
-- show helpful error messages instead of swallowing them.
evalTime ∷ ∀ m . TimeQuery ~> DSL m
evalTime (UpdateCommand command val next) = do
  {time} <- H.get
  let time' = Int.fromString val >>= \n -> updateTime command n time

  case time' of
    Just time'' -> do
      H.modify _{ time = time'' }
      H.raise (NotifyChange time'')
    Nothing -> D.traceAnyA {val, command, msg: "parsing val or updating time has failed"}
  pure next

updateTime :: F.Command -> Int -> Time -> Maybe Time
updateTime F.Hours24 n t = toEnum n <#> ( _ `setHour` t)
updateTime F.Hours12 n t = toEnum n >>= (_ `setHour12` t)
updateTime F.Meridiem n t = toEnum n >>= (_ `setMeridiem` t)
updateTime F.MinutesTwoDigits n t = toEnum n <#> ( _ `setMinute` t)
updateTime F.Minutes n t = toEnum n <#> ( _ `setMinute` t)
updateTime F.SecondsTwoDigits n t = toEnum n <#> ( _ `setSecond` t)
updateTime F.Seconds n t = toEnum n <#> ( _ `setSecond` t)
updateTime F.Milliseconds n t =toEnum n <#>  (_ `setMillisecond` t)
updateTime F.MillisecondsTwoDigits n t = toEnum n >>= (_ `setMillisecond2` t)
updateTime F.MillisecondsShort n t = toEnum n >>= (_ `setMillisecond2` t)
updateTime (F.Placeholder _) _ t = pure t

evalPicker ∷ ∀ m . (PickerQuery Time) ~> DSL m
evalPicker (SetValue time next) = do
  H.modify _{ time = time }
  H.raise (NotifyChange time)
  pure next
evalPicker (GetValue next) = do
  H.gets _.time <#> next
