module Halogen.Datapicker.Component.Duration where

import Prelude
import Debug.Trace as D
import Halogen as H
import Halogen.Datapicker.Component.Duration.Format as F
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Data.Foldable (foldMap)
import Data.Functor.Coproduct (Coproduct, coproduct, right)
import Data.Interval (Duration, IsoDuration, mkIsoDuration, unIsoDuration)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Number (fromString)
import Data.String (Pattern(..), stripSuffix)
import Halogen.Datapicker.Component.Internal.Elements (numberElement, minRange)
import Halogen.Datapicker.Component.Types (PickerQuery(..), PickerMessage(..))


data DurationQuery a = UpdateCommand F.Command String a

type Query = Coproduct (PickerQuery Unit IsoDuration) DurationQuery
type Message = PickerMessage IsoDuration

type State =
  { format :: F.Format
  , duration :: IsoDuration
  }


type DSL = H.ComponentDSL State Query Message
type HTML = H.ComponentHTML DurationQuery


picker ∷ ∀ m. F.Format -> IsoDuration -> H.Component HH.HTML Query Unit Message m
picker format duration = H.component
  { initialState: const {format, duration}
  , render: render <#> (map right)
  , eval: coproduct evalPicker evalDuration
  , receiver: const Nothing
  }

render ∷ State -> HTML
render {duration, format} = HH.ul [HP.classes [HH.ClassName "Picker"]] $
  foldMap (pure <<< f) (unwrap format)
  where
  f cmd = HH.li [HP.classes [HH.ClassName "Picker-component"]] [renderCommand duration cmd]

renderCommand :: IsoDuration -> F.Command -> HTML
renderCommand d cmd =
  numberElement
    (UpdateCommand cmd)
    { title: show cmd, range: minRange 0.0 }
    (showNum $ getComponent cmd d)
  where
  showNum 0.0 = "0"
  showNum n = let str = show n
    in maybe str id (stripSuffix (Pattern ".0") str)

getComponent :: F.Command -> IsoDuration -> Number
getComponent cmd d = maybe 0.0 id $ F.toGetter cmd (unIsoDuration d)

applyChange :: F.Command -> Number -> Duration -> Duration
applyChange cmd val dur = F.toSetter cmd val dur


evalDuration ∷ ∀ m . DurationQuery ~> DSL m
evalDuration (UpdateCommand cmd val next) = do
  {duration} <- H.get
  let newDur = applyChange cmd <$> fromString val <*> (pure $ unIsoDuration duration) >>= mkIsoDuration
  case newDur of
    Just dur -> H.modify _{ duration = dur } *> H.raise (NotifyChange dur)
    Nothing -> D.traceAnyA {duration, cmd, val, msg: "apply of the change produced invlaid ISO duration"}
  pure next

evalPicker ∷ ∀ m . (PickerQuery Unit IsoDuration) ~> DSL m
evalPicker (SetValue duration next) = do
  H.modify _{ duration = duration }
  pure $ next unit
evalPicker (GetValue next) = H.gets _.duration <#> next
