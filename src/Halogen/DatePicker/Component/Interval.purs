module Halogen.Datapicker.Component.Interval where

import Prelude
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.Datapicker.Component.DateTime as DateTime
import Halogen.Datapicker.Component.DateTime.Format as DateTimeF
import Halogen.Datapicker.Component.Duration as Duration
import Halogen.Datapicker.Component.Duration.Format as DurationF
import Halogen.Datapicker.Component.Interval.Format as F
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Data.Bifunctor (bimap, lmap)
import Data.Date (canonicalDate)
import Data.DateTime (DateTime(..))
import Data.Either.Nested (Either2)
import Data.Enum (toEnum)
import Data.Functor.Coproduct (Coproduct, coproduct, right, left)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Interval (Interval(..), IsoDuration, mkIsoDuration, millisecond)
import Data.Maybe (Maybe(..), fromJust)
import Halogen.Datapicker.Component.Types (PickerQuery(..), PickerMessage(..))
import Partial.Unsafe (unsafePartial, unsafePartialBecause)

type FullInterval = Interval IsoDuration DateTime

data IntervalQuery a
  = HandleDurationMessage Duration.Message a
  | HandleDateTimeMessage Boolean DateTime.Message a

type Query = Coproduct (PickerQuery FullInterval) IntervalQuery
type Message = PickerMessage (FullInterval)
type State =
  { format :: F.Format
  , interval :: FullInterval
  }

type ChildQuery = Coproduct2 Duration.Query DateTime.Query
type Slot = Either2 Unit Boolean

cpDuration ∷ CP.ChildPath Duration.Query ChildQuery Unit Slot
cpDuration = CP.cp1
cpDateTime ∷ CP.ChildPath DateTime.Query ChildQuery Boolean Slot
cpDateTime = CP.cp2


type HTML m = H.ParentHTML IntervalQuery ChildQuery Slot m
type DSL m = H.ParentDSL State Query ChildQuery Slot Message m


initialStateFromFormat ∷ F.Format -> State
initialStateFromFormat format =
  { format: format
  , interval: case format of
      StartEnd      _ _ -> StartEnd dateTime dateTime
      DurationEnd   _ _ -> DurationEnd duration dateTime
      StartDuration _ _ -> StartDuration dateTime duration
      JustDuration  _ -> JustDuration duration
  }
  where
    dateTime = DateTime (canonicalDate year bottom bottom) bottom
    year = unsafePartialBecause "unreachable as `0` year is in bounds" fromJust $ toEnum 0
    duration = unsafePartial fromJust $ mkIsoDuration $ millisecond 0.0


picker ∷ ∀ m. F.Format -> H.Component HH.HTML Query Unit  Message m
picker fmt = H.parentComponent
  { initialState: const $ initialStateFromFormat fmt
  , render: render >>> bimap (map right) right
  , eval: coproduct evalPicker evalInterval
  , receiver: const Nothing
  }
  where
  render ∷ State -> HTML m
  render {interval, format} = HH.ul_ $ case format of
    StartEnd a b ->
      [ HH.li_ [renderDateTime a false]
      , HH.li_ [HH.text "/"]
      , HH.li_ [renderDateTime b true]]
    DurationEnd d a ->
      [ HH.li_ [renderDuration d]
      , HH.li_ [HH.text "/"]
      , HH.li_ [renderDateTime a false]]
    StartDuration a d ->
      [ HH.li_ [renderDateTime a false]
      , HH.li_ [HH.text "/"]
      , HH.li_ [renderDuration d]]
    JustDuration d ->
      [ HH.li_ [renderDuration d]]

renderDuration :: ∀ m. DurationF.Format -> HTML m
renderDuration fmt = HH.slot' cpDuration unit (Duration.picker fmt) unit (HE.input $ HandleDurationMessage)
renderDateTime :: ∀ m. DateTimeF.Format -> Boolean -> HTML m
renderDateTime fmt idx = HH.slot' cpDateTime idx (DateTime.picker fmt) unit (HE.input $ HandleDateTimeMessage idx)


evalInterval ∷ ∀ m . IntervalQuery ~> DSL m
evalInterval (HandleDateTimeMessage idx msg next) = do
  {interval} <- H.get
  let
    newInterval = case msg of
      NotifyChange newDateTime -> case interval of
        StartEnd a b -> case idx of
          true -> StartEnd newDateTime b
          false -> StartEnd a newDateTime
        DurationEnd d a -> DurationEnd d newDateTime
        StartDuration a d -> StartDuration newDateTime d
        JustDuration d -> JustDuration d
  H.modify _{ interval = newInterval }
  H.raise (NotifyChange newInterval)
  pure next
evalInterval (HandleDurationMessage msg next) = do
  {interval} <- H.get
  let
    newInterval = case msg of
      NotifyChange newDuration -> lmap (const newDuration) interval
  H.modify _{ interval = newInterval }
  H.raise (NotifyChange newInterval)
  pure next


-- toShape = bimap (const unit) (const unit)

evalPicker ∷ ∀ m . (PickerQuery FullInterval) ~> DSL m
evalPicker (SetValue intervalNew next) = do
  {interval} <- H.get
  -- TODO reis error if new interval has no shape of old one (or format)?
  -- guard (toShape interval /= toShape intervalNew)

  H.modify _{ interval = intervalNew }
  case intervalNew of
    StartEnd a b -> setDateTime false a *> setDateTime true b
    DurationEnd d a -> setDuration d *> setDateTime false a
    StartDuration a d -> setDateTime false a *> setDuration d
    JustDuration d -> setDuration d
  H.raise (NotifyChange intervalNew)
  pure next
  where
  setDuration :: IsoDuration -> DSL m Unit
  setDuration val = void $ H.query' cpDuration unit $ H.action $ left <<< (SetValue val)
  setDateTime :: Boolean -> DateTime -> DSL m Unit
  setDateTime idx val = void $ H.query' cpDateTime idx $ H.action $ left <<< (SetValue val)
evalPicker (GetValue next) = do
  H.gets _.interval <#> next
