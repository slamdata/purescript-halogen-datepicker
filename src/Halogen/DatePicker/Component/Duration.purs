module Halogen.Datepicker.Component.Duration where

import Prelude

import Data.Array (fold)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Functor.Coproduct (Coproduct, coproduct, right, left)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Interval (Duration, IsoDuration, mkIsoDuration, unIsoDuration)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (mempty)
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (unwrap)
import Data.Traversable (for)
import Halogen as H
import Halogen.Datepicker.Component.Types (BasePickerQuery(..), PickerMessage(..), PickerQuery(..), PickerValue)
import Halogen.Datepicker.Format.Duration as F
import Halogen.Datepicker.Internal.Num as N
import Halogen.Datepicker.Internal.Range (minRange)
import Halogen.Datepicker.Internal.Utils (foldSteps, componentProps, transitionState', asRight, mustBeMounted, pickerProps)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE


data DurationQuery a = UpdateCommand F.Command (Maybe Number) a
type State = PickerValue DurationError IsoDuration
type QueryIn = PickerQuery Unit State
type Query = Coproduct QueryIn DurationQuery
type Message = PickerMessage State

data DurationError = InvalidIsoDuration
derive instance durationErrorEq ∷ Eq DurationError
derive instance durationErrorOrd ∷ Ord DurationError
derive instance durationErrorGeneric ∷ Generic DurationError _
instance durationErrorShow ∷ Show DurationError where
  show = genericShow

type Slot = F.Command


type HTML m = H.ParentHTML DurationQuery (N.Query Number) Slot m
type DSL m = H.ParentDSL State Query (N.Query Number) Slot Message m


picker ∷ ∀ m. F.Format → H.Component HH.HTML Query Unit Message m
picker format = H.parentComponent
  { initialState: const Nothing
  , render: render format >>> bimap (map right) right
  , eval: coproduct (evalPicker format) (evalDuration format)
  , receiver: const Nothing
  }

render ∷  ∀ m. F.Format → State → HTML m
render format duration = HH.ul (pickerProps duration) (unwrap format <#> renderCommand)

renderCommand ∷ ∀ m. F.Command → HTML m
renderCommand cmd = HH.li componentProps
  [ HH.slot
    cmd
    (N.picker N.numberHasNumberInputVal { title: show cmd, range: minRange 0.0 })
    unit
    (HE.input $ \(NotifyChange n) → UpdateCommand cmd n)]

getComponent ∷ F.Command → IsoDuration → Number
getComponent cmd d = fromMaybe 0.0 $ F.toGetter cmd (unIsoDuration d)

overIsoDuration ∷ (Duration → Duration) → IsoDuration → Maybe IsoDuration
overIsoDuration f d = mkIsoDuration $ f $ unIsoDuration d

evalDuration ∷ ∀ m. F.Format → DurationQuery ~> DSL m
evalDuration format (UpdateCommand cmd val next) = do
  transitionState' InvalidIsoDuration case _ of
    Just (Right prevDuration) → pure
      $ maybe (Left false) Right
      $ val >>= \n → overIsoDuration (F.toSetter cmd n) prevDuration
    _  → buildDuration format
  pure next

type BuildStep = Maybe (Endo Duration)
buildDuration ∷ ∀ m. F.Format → DSL m (Either Boolean IsoDuration)
buildDuration format = do
  steps ← for (unwrap format) mkBuildStep
  pure case runStep $ foldSteps steps of
   Just (Just x) → Right x
   Just Nothing → Left true
   Nothing → Left false
  where
  mkBuildStep ∷ F.Command → DSL m BuildStep
  mkBuildStep cmd = do
    num ← H.query cmd $ H.request (left <<< GetValue)
    pure $ join num <#> F.toSetter cmd >>> Endo
  runStep ∷ BuildStep -> Maybe (Maybe IsoDuration)
  runStep step = step <#> \(Endo f) -> mkIsoDuration $ f mempty


evalPicker ∷ ∀ m. F.Format → QueryIn ~> DSL m
evalPicker _ (ResetError next) = do
  H.put Nothing
  pure next
evalPicker format (Base (SetValue duration reply)) = do
  propagateChange format duration
  H.put duration
  pure $ reply unit
evalPicker _ (Base (GetValue reply)) = H.get <#> reply

propagateChange ∷ ∀ m . F.Format → State → DSL m Unit
propagateChange format duration = do
  map (mustBeMounted <<< fold) $ for (unwrap format) \cmd → do
    let n = duration >>= asRight >>= (F.toGetter cmd <<< unIsoDuration)
    H.query cmd $ H.request $ left <<< SetValue n
