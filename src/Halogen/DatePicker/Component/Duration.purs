module Halogen.Datepicker.Component.Duration where

import Prelude

import Data.Array (fold)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Functor.Coproduct (Coproduct, coproduct, right)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Interval (Duration)
import Data.Interval.Duration.Iso (IsoDuration, mkIsoDuration, unIsoDuration, Errors)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (mempty)
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (unwrap)
import Data.String (take)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.Datepicker.Component.Types (BasePickerQuery(..), PickerMessage, PickerQuery(..), PickerValue)
import Halogen.Datepicker.Config (Config, defaultConfig)
import Halogen.Datepicker.Format.Duration as F
import NumberInput.Halogen.Component as Num
import NumberInput.Range (minRange)
import Halogen.Datepicker.Internal.Utils (mapParentHTMLQuery, foldSteps, componentProps, transitionState, asRight, mustBeMounted, pickerProps)
import Halogen.Datepicker.Internal.Elements (toNumConf)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE


type State = PickerValue DurationError IsoDuration

type Message = PickerMessage State

type Query = Coproduct QueryIn DurationQuery
type QueryIn = PickerQuery Unit State
data DurationQuery a = UpdateCommand F.Command (Maybe Number) a

data DurationError = InvalidIsoDuration (Maybe Errors)
derive instance durationErrorEq ∷ Eq DurationError
derive instance durationErrorOrd ∷ Ord DurationError
derive instance durationErrorGeneric ∷ Generic DurationError _
instance durationErrorShow ∷ Show DurationError where
  show = genericShow

type Slot = F.Command
type ChildQuery = Num.Query Number

type HTML m = H.ParentHTML DurationQuery ChildQuery Slot m
type DSL m = H.ParentDSL State Query ChildQuery Slot Message m

picker ∷ ∀ m. F.Format → H.Component HH.HTML Query Unit Message m
picker = pickerWithConfig defaultConfig

pickerWithConfig ∷ ∀ m. Config → F.Format → H.Component HH.HTML Query Unit Message m
pickerWithConfig config format = H.parentComponent
  { initialState: const Nothing
  , render: render config format >>> mapParentHTMLQuery right
  , eval: coproduct (evalPicker format) (evalDuration format)
  , receiver: const Nothing
  }

render ∷ ∀ m. Config → F.Format → State → HTML m
render config format duration = HH.ul (pickerProps config duration) (unwrap format <#> renderCommand config)

renderCommand ∷ ∀ m. Config → F.Command → HTML m
renderCommand config cmd = HH.li (componentProps config)
  [ HH.slot
    cmd
    (Num.input Num.numberHasNumberInputVal $ toNumConf config { title: show cmd, placeholder: take 1 (show cmd),  range: minRange 0.0 })
    unit
    (HE.input $ \(Num.NotifyChange n) → UpdateCommand cmd n)]

getComponent ∷ F.Command → IsoDuration → Number
getComponent cmd d = fromMaybe 0.0 $ F.toGetter cmd (unIsoDuration d)

overIsoDuration ∷ (Duration → Duration) → IsoDuration → Either Errors IsoDuration
overIsoDuration f d = mkIsoDuration $ f $ unIsoDuration d

evalDuration ∷ ∀ m. F.Format → DurationQuery ~> DSL m
evalDuration format (UpdateCommand cmd val next) = do
  transitionState case _ of
    Just (Right prevDuration) → pure case val of
      Just n → overIsoDuration (F.toSetter cmd n) prevDuration # lmap \err ->
        Tuple false (InvalidIsoDuration (Just err))
      Nothing -> Left (Tuple false (InvalidIsoDuration Nothing))
    _  → buildDuration format
  pure next

type BuildStep = Maybe (Endo Duration)
buildDuration ∷ ∀ m
  . F.Format
  → DSL m (Either (Tuple Boolean DurationError) IsoDuration)
buildDuration format = do
  steps ← for (unwrap format) mkBuildStep
  pure case runStep $ foldSteps steps of
   Just (Right x) → Right x
   Just (Left err) → Left (Tuple false (InvalidIsoDuration (Just err)))
   Nothing → Left (Tuple false (InvalidIsoDuration Nothing))
  where
  mkBuildStep ∷ F.Command → DSL m BuildStep
  mkBuildStep cmd = do
    num ← query cmd $ H.request (Num.GetValue)
    pure $ num <#> F.toSetter cmd >>> Endo
  runStep ∷ BuildStep -> Maybe (Either Errors IsoDuration)
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
  map fold $ for (unwrap format) \cmd → do
    let n = duration >>= asRight >>= unIsoDuration >>> F.toGetter cmd
    query cmd $ H.action $ Num.SetValue n

query ∷ ∀ m. Slot → ChildQuery ~> DSL m
query cmd q = H.query cmd q >>= mustBeMounted
