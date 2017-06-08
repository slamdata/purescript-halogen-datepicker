module Halogen.Datapicker.Component.Duration where

import Prelude
import Halogen as H
import Halogen.Datapicker.Component.Duration.Format as F
import Halogen.Datapicker.Component.Internal.Number as N
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Control.MonadPlus (guard)
import Data.Array (fold)
import Data.Bifunctor (bimap)
import Data.Either (Either(..), either)
import Data.Foldable (foldMap)
import Data.Functor.Coproduct (Coproduct, coproduct, right, left)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Interval (Duration, IsoDuration, mkIsoDuration, unIsoDuration)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (unwrap)
import Data.Traversable (for, sequence)
import Halogen.Datapicker.Component.Internal.Elements (minRange)
import Halogen.Datapicker.Component.Types (PickerMessage(..), PickerQuery(..), PickerValue, isInvalid, mustBeMounted, value)


data DurationQuery a = UpdateCommand F.Command (Maybe Number) a
type Input = PickerValue DurationError IsoDuration
type QueryIn = PickerQuery Unit Input
type Query = Coproduct QueryIn DurationQuery
type Message = PickerMessage Input

data DurationError = InvalidIsoDuration
derive instance durationErrorEq :: Eq DurationError
derive instance durationErrorOrd :: Ord DurationError
derive instance durationErrorGeneric :: Generic DurationError _
instance durationErrorShow :: Show DurationError where
  show = genericShow

type State =
  { format :: F.Format
  , duration :: Input
  }

type Slot = F.Command


type HTML m = H.ParentHTML DurationQuery N.Query Slot m
type DSL m = H.ParentDSL State Query N.Query Slot Message m


picker ∷ ∀ m. F.Format -> H.Component HH.HTML Query Unit Message m
picker format = H.parentComponent
  { initialState: const $ {format, duration: Nothing}
  , render: render >>> bimap (map right) right
  , eval: coproduct evalPicker evalDuration
  , receiver: const Nothing
  }

render ∷  ∀ m. State -> HTML m
render s =
  HH.ul
    [ HP.classes $
      [ HH.ClassName "Picker" ]
      <> (guard (isInvalid s.duration) $> HH.ClassName "Picker--invalid")
    ]
    (foldMap (pure <<< f) (unwrap s.format))
  where
  f cmd = HH.li [ HP.classes [ HH.ClassName "Picker-component" ] ]
    [ HH.slot
      cmd
      (N.picker { title: show cmd, range: minRange 0.0 })
      unit
      (HE.input $ \(NotifyChange n) -> UpdateCommand cmd n)]

getComponent :: F.Command -> IsoDuration -> Number
getComponent cmd d = maybe 0.0 id $ F.toGetter cmd (unIsoDuration d)

overIsoDuration :: (Duration -> Duration) -> IsoDuration -> Maybe IsoDuration
overIsoDuration f d = mkIsoDuration $ f $ unIsoDuration d

evalDuration ∷ ∀ m . DurationQuery ~> DSL m
evalDuration (UpdateCommand cmd val next) = do
  s <- H.get
  let durationValue = value s.duration
  nextDurationValue <- case durationValue of
    Nothing -> buildDuration
    Just dur -> pure $ val >>= \n -> overIsoDuration (F.toSetter cmd n) dur
  let
    nextDuration = case s.duration, nextDurationValue of
      _, Just x -> Just (Right x)
      Just _, Nothing -> Just (Left InvalidIsoDuration)
      Nothing, Nothing -> Nothing
  H.modify _{duration = nextDuration}
  when (nextDuration /= s.duration) $ H.raise (NotifyChange nextDuration)
  pure next

buildDuration :: ∀ m. DSL m (Maybe IsoDuration)
buildDuration = do
  {format} <- H.get
  mbEndo <- for (unwrap format) \cmd -> do
    num <- H.query cmd $ H.request (left <<< GetValue)
    pure case num of
      Just (Just n) -> Just $ Endo $ F.toSetter cmd n
      _ -> Nothing
  pure case map fold $ sequence mbEndo of
   Just (Endo f) -> mkIsoDuration $ f mempty
   _ -> Nothing


evalPicker ∷ ∀ m . QueryIn ~> DSL m
evalPicker (SetValue duration next) = do
  {format} <- H.get
  propagateChange format duration
  H.modify \s -> s{duration = duration}
  pure $ next unit
evalPicker (GetValue next) = H.gets _.duration <#> next

propagateChange :: ∀ m . F.Format -> Input -> DSL m Unit
propagateChange format duration = do
  map (mustBeMounted <<< fold) $ for (unwrap format) \cmd -> do
    let n = (duration >>= either (const Nothing) (F.toGetter cmd <<< unIsoDuration)) :: Maybe Number
    H.query cmd $ H.request $ left <<< SetValue n
