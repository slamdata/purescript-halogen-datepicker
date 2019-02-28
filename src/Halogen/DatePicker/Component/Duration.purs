module Halogen.Datepicker.Component.Duration where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Data.Array (fold)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Interval (Duration)
import Data.Interval.Duration.Iso (IsoDuration, mkIsoDuration, unIsoDuration, Errors)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (unwrap)
import Data.String (take)
import Data.Symbol (SProxy(..))
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Effect.Exception as Ex
import Halogen as H
import Halogen.Datepicker.Component.Types (BasePickerQuery(..), PickerQuery, PickerValue)
import Halogen.Datepicker.Config (Config, defaultConfig)
import Halogen.Datepicker.Format.Duration as F
import Halogen.Datepicker.Internal.Elements (toNumConf)
import Halogen.Datepicker.Internal.Num as Num
import Halogen.Datepicker.Internal.Range (minRange)
import Halogen.Datepicker.Internal.Utils (asRight, componentProps, foldSteps, handlePickerQuery, mustBeMounted, pickerProps, transitionState)
import Halogen.HTML as HH

type State = PickerValue DurationError IsoDuration

type Message = PickerValue DurationError IsoDuration

type Query = PickerQuery Unit State
type Action = Tuple F.Command (Maybe Number)

data DurationError = InvalidIsoDuration (Maybe Errors)
derive instance durationErrorEq ∷ Eq DurationError
derive instance durationErrorOrd ∷ Ord DurationError
derive instance durationErrorGeneric ∷ Generic DurationError _
instance durationErrorShow ∷ Show DurationError where show = genericShow

type Slots = (num ∷ Num.Slot Number F.Command)

_num = SProxy ∷ SProxy "num"

type Slot = H.Slot Query Message

type HTML m = H.ComponentHTML Action Slots m
type DSL = H.HalogenM State Action Slots Message

picker ∷ ∀ m. MonadError Ex.Error m ⇒ F.Format → H.Component HH.HTML Query Unit Message m
picker = pickerWithConfig defaultConfig

pickerWithConfig
  ∷ ∀ m
  . MonadError Ex.Error m
  ⇒ Config
  → F.Format
  → H.Component HH.HTML Query Unit Message m
pickerWithConfig config format =
  H.mkComponent
    { initialState: const Nothing
    , render: render config format
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction format
        , handleQuery = handlePickerQuery (propagateChange format)
        }
    }

render ∷ ∀ m. Config → F.Format → State → HTML m
render config format duration = HH.ul (pickerProps config duration) (unwrap format <#> renderCommand config)

renderCommand ∷ ∀ m. Config → F.Command → HTML m
renderCommand config cmd =
  HH.li (componentProps config)
    [ HH.slot
        _num
        cmd
        (Num.picker Num.numberHasNumberInputVal $ toNumConf config { title: show cmd, placeholder: take 1 (show cmd),  range: minRange 0.0 })
        unit
        (\n → Just (Tuple cmd n))
    ]

getComponent ∷ F.Command → IsoDuration → Number
getComponent cmd d = fromMaybe 0.0 $ F.toGetter cmd (unIsoDuration d)

overIsoDuration ∷ (Duration → Duration) → IsoDuration → Either Errors IsoDuration
overIsoDuration f d = mkIsoDuration $ f $ unIsoDuration d

handleAction ∷ ∀ m. MonadError Ex.Error m ⇒ F.Format → Action → DSL m Unit
handleAction format (Tuple cmd val) = do
  transitionState case _ of
    Just (Right prevDuration) → pure case val of
      Just n → overIsoDuration (F.toSetter cmd n) prevDuration # lmap \err ->
        Tuple false (InvalidIsoDuration (Just err))
      Nothing → Left (Tuple false (InvalidIsoDuration Nothing))
    _  → buildDuration format

type BuildStep = Maybe (Endo (->) Duration)
buildDuration
  ∷ ∀ m
  . MonadError Ex.Error m
  ⇒ F.Format
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
    num ← query cmd $ H.request GetValue
    pure $ num <#> F.toSetter cmd >>> Endo
  runStep ∷ BuildStep → Maybe (Either Errors IsoDuration)
  runStep step = step <#> \(Endo f) → mkIsoDuration $ f mempty

propagateChange ∷ ∀ m. MonadError Ex.Error m ⇒ F.Format → State → DSL m Unit
propagateChange format duration = do
  map fold $ for (unwrap format) \cmd → do
    let n = duration >>= asRight >>= unIsoDuration >>> F.toGetter cmd
    query cmd $ H.request (SetValue n)

query ∷ ∀ m. MonadError Ex.Error m ⇒ F.Command → Num.Query Number ~> DSL m
query cmd q = H.query _num cmd q >>= mustBeMounted
