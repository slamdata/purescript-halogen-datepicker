module Halogen.Datapicker.Component.Duration where

import Prelude
import Halogen as H
import Halogen.Datapicker.Component.Duration.Format as F
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Control.MonadPlus (guard)
import Data.Array (fold)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Functor.Coproduct (Coproduct, coproduct, right)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Interval (Duration, IsoDuration, mkIsoDuration, unIsoDuration)
import Data.List (List)
import Data.Map (Map, empty, fromFoldable, insert, lookup, toUnfoldable)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Halogen.Datapicker.Component.Internal.Elements (NumberInputValue, emptyNumberInputValue, minRange, mkNumberInputValue, num, numberElement, zeroNumberInputValue)
import Halogen.Datapicker.Component.Types (PickerQuery(..), PickerMessage(..), PickerValue, isInvalid, value)


data DurationQuery a = UpdateCommand F.Command NumberInputValue a

type QueryIn = PickerQuery Unit (PickerValue DurationError IsoDuration)
type Query = Coproduct QueryIn DurationQuery
type Message = PickerMessage (PickerValue DurationError IsoDuration)

data DurationError = InvalidIsoDuration
derive instance durationErrorGeneric :: Generic DurationError _
derive instance durationErrorEq :: Eq DurationError
derive instance durationErrorOrd :: Ord DurationError
instance durationErrorShow :: Show DurationError where
  show = genericShow

type Values = Map F.Command NumberInputValue
type State =
  { format :: F.Format
  , duration :: PickerValue DurationError IsoDuration
  , vals :: Values
  }


type DSL = H.ComponentDSL State Query Message
type HTML = H.ComponentHTML DurationQuery

durationToVals :: Values -> F.Format -> Maybe IsoDuration -> Values
durationToVals vals format isoDuration = cleanVals
  where
  cleanVals :: Values
  cleanVals =  fromFoldable $ unwrap format <#> mapper (map unIsoDuration isoDuration)
  mapper :: Maybe Duration -> F.Command -> Tuple F.Command NumberInputValue
  mapper Nothing cmd = Tuple cmd $ maybe
    emptyNumberInputValue
    id
    (lookup cmd vals)
  mapper (Just duration) cmd = Tuple cmd $ case lookup cmd vals, F.toGetter cmd duration of
    Just old, Just new -> if num old == Just new then old else mkNumberInputValue new
    Just old, Nothing -> old
    Nothing, Just new -> mkNumberInputValue new
    Nothing, Nothing -> zeroNumberInputValue

picker ∷ ∀ m. F.Format -> IsoDuration -> H.Component HH.HTML Query Unit Message m
picker format duration = H.component
  { initialState: const $ {format, duration: Just (Right duration), vals: durationToVals empty format (Just duration)}
  , render: render <#> (map right)
  , eval: coproduct evalPicker evalDuration
  , receiver: const Nothing
  }

render ∷ State -> HTML
render s =
  HH.ul
    [ HP.classes $
      [ HH.ClassName "Picker" ]
      <> (guard (isInvalid s.duration) $> HH.ClassName "Picker--invalid")
    ]
    (foldMap (pure <<< f) (unwrap s.format))
  where
  f cmd = HH.li
    [ HP.classes [ HH.ClassName "Picker-component" ] ]
    [renderCommand cmd (lookup cmd s.vals)]

renderCommand :: F.Command -> Maybe NumberInputValue -> HTML
renderCommand cmd num =
  numberElement
    (UpdateCommand cmd)
    { title: show cmd
    , range: minRange 0.0
    }
    (maybe (Tuple (Just 0.0) "0") id num)

getComponent :: F.Command -> IsoDuration -> Number
getComponent cmd d = maybe 0.0 id $ F.toGetter cmd (unIsoDuration d)

applyChange :: F.Command -> Number -> Duration -> Duration
applyChange cmd val dur = F.toSetter cmd val dur

overIsoDuration :: (Duration -> Duration) -> IsoDuration -> Maybe IsoDuration
overIsoDuration f d = mkIsoDuration $ f $ unIsoDuration d

buildDuration :: Map F.Command NumberInputValue -> Maybe IsoDuration
buildDuration vals =
  mbEndo >>= (\(Endo f) -> mkIsoDuration $ f mempty)
  where
  mbEndo :: Maybe (Endo Duration)
  mbEndo = map fold $ traverse
    (\(Tuple cmd (Tuple num _)) -> (Endo <<< applyChange cmd) <$> num)
    (toUnfoldable vals :: List (Tuple F.Command NumberInputValue))


evalDuration ∷ ∀ m . DurationQuery ~> DSL m
evalDuration (UpdateCommand cmd val next) = do
  s <- H.get
  let
    vals' = insert cmd val s.vals
    durationValue = value s.duration
    nextDurationValue = case durationValue of
      Nothing -> buildDuration vals'
      Just duration' -> (num val) >>= \n -> overIsoDuration (applyChange cmd n) duration'
    vals'' = durationToVals vals' s.format nextDurationValue
    nextDuration = case s.duration, nextDurationValue of
      _, Just x -> Just (Right x)
      Just _, Nothing -> Just (Left InvalidIsoDuration)
      Nothing, Nothing -> Nothing
  H.modify _{duration = nextDuration, vals = vals''}
  when (nextDuration /= s.duration) $ H.raise (NotifyChange nextDuration)
  pure next


evalPicker ∷ ∀ m . QueryIn ~> DSL m
evalPicker (SetValue duration next) = do
  H.modify \s -> s{duration = duration, vals = durationToVals empty s.format (value duration)}
  pure $ next unit
evalPicker (GetValue next) = H.gets _.duration <#> next
