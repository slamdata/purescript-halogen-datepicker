module Halogen.Datapicker.Component.Time where

import Prelude
import Data.Int as Int
import Debug.Trace as D
import Halogen as H
import Halogen.Datapicker.Component.Time.Format as F
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Data.Enum (toEnum)
import Data.Foldable (foldMap)
import Data.Functor.Coproduct (Coproduct, coproduct, right)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Time (Time, hour, millisecond, minute, second, setHour, setMillisecond, setMinute, setSecond)
import Halogen.Datapicker.Component.Internal.Elements (textElement, enumElement, choiceElement)
import Halogen.Datapicker.Component.Internal.Enums (hour12, meridiem, millisecond2, millisecond1, setHour12, setMeridiem, setMillisecond2, setMillisecond1)
import Halogen.Datapicker.Component.Types (PickerQuery(..), PickerMessage(..))

data TimeQuery a = UpdateCommand F.Command String a

type Query = Coproduct (PickerQuery Unit Time) TimeQuery
type Message = PickerMessage Time
type State =
  { format :: F.Format
  , time :: Time
  }

type DSL = H.ComponentDSL State Query Message
type HTML = H.ComponentHTML TimeQuery

picker ∷ ∀ m. F.Format -> Time -> H.Component HH.HTML Query Unit Message m
picker format time = H.component
  { initialState: const $ {format, time}
  , render: render <#> (map right)
  , eval: coproduct evalPicker evalTime
  , receiver: const Nothing
  }

render ∷ State -> HTML
render {time, format} = HH.ul [HP.classes [HH.ClassName "Picker"]] $
  foldMap (pure <<< f) (unwrap format)
  where
  f cmd = HH.li [HP.classes [HH.ClassName "Picker-component"]] [renderCommand time cmd]

renderCommand :: Time -> F.Command -> HTML
renderCommand t cmd@F.Hours24               = enumElement (UpdateCommand cmd) { title: "Hours"} (hour t)
renderCommand t cmd@F.Hours12               = enumElement (UpdateCommand cmd) { title: "Hours"} (hour12 t)
renderCommand t cmd@F.Meridiem              = choiceElement (UpdateCommand cmd) { title: "Meridiem" } (meridiem t)
renderCommand t cmd@F.MinutesTwoDigits      = enumElement (UpdateCommand cmd) { title: "Minutes"} (minute t)
renderCommand t cmd@F.Minutes               = enumElement (UpdateCommand cmd) { title: "Minutes"} (minute t)
renderCommand t cmd@F.SecondsTwoDigits      = enumElement (UpdateCommand cmd) { title: "Seconds"} (second t)
renderCommand t cmd@F.Seconds               = enumElement (UpdateCommand cmd) { title: "Seconds"} (second t)
renderCommand t cmd@F.Milliseconds          = enumElement (UpdateCommand cmd) { title: "Milliseconds"} (millisecond t)
renderCommand t cmd@F.MillisecondsTwoDigits = enumElement (UpdateCommand cmd) { title: "Milliseconds"} (millisecond2 t)
renderCommand t cmd@F.MillisecondsShort     = enumElement (UpdateCommand cmd) { title: "Milliseconds"} (millisecond1 t)
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
updateTime F.MillisecondsShort n t = toEnum n >>= (_ `setMillisecond1` t)
updateTime (F.Placeholder _) _ t = pure t

evalPicker ∷ ∀ m . (PickerQuery Unit Time) ~> DSL m
evalPicker (SetValue time next) = do
  H.modify _{ time = time }
  pure $ next unit
evalPicker (GetValue next) = H.gets _.time <#> next
