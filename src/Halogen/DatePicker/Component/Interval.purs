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
import Halogen.HTML.Properties as HP
import Data.Bifunctor (class Bifunctor, bimap, lmap)
import Data.DateTime (DateTime)
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct (Coproduct, coproduct, right, left)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Interval (Interval(..), IsoDuration)
import Data.Maybe (Maybe(..))
import Halogen.Datapicker.Component.Internal.Elements (textElement)
import Halogen.Datapicker.Component.Types (PickerMessage(..), PickerQuery(..), mustBeMounted)

type Input = Interval IsoDuration DateTime
data IntervalError = IntervalIsNotInShapeOfFormat

data IntervalQuery a
  = HandleDurationMessage Duration.Message a
  | HandleDateTimeMessage Boolean DateTime.Message a

type Query = Coproduct (PickerQuery (Maybe IntervalError) Input) IntervalQuery
type Message = PickerMessage (Input)
type State =
  { format :: F.Format
  , interval :: Input
  }

type ChildQuery = Coproduct2 Duration.Query DateTime.Query
type Slot = Either2 Unit Boolean

cpDuration ∷ CP.ChildPath Duration.Query ChildQuery Unit Slot
cpDuration = CP.cp1
cpDateTime ∷ CP.ChildPath DateTime.Query ChildQuery Boolean Slot
cpDateTime = CP.cp2


type HTML m = H.ParentHTML IntervalQuery ChildQuery Slot m
type DSL m = H.ParentDSL State Query ChildQuery Slot Message m


picker ∷ ∀ m. F.Format -> Input -> H.Component HH.HTML Query Unit Message m
picker format interval = H.parentComponent
  { initialState: const $ {format, interval}
  , render: render >>> bimap (map right) right
  , eval: coproduct evalPicker evalInterval
  , receiver: const Nothing
  }

render ∷ ∀ m. State -> HTML m
render {interval, format} = HH.div [HP.classes [HH.ClassName "Picker"]] $ case format, interval of
  StartEnd fmtStart fmtEnd, StartEnd start end ->
    [ HH.div [HP.classes [HH.ClassName "Picker-component"]] $ pure $ renderDateTime fmtStart start false
    , HH.div [HP.classes [HH.ClassName "Picker-component"]] $ pure $ textElement { text: "/" }
    , HH.div [HP.classes [HH.ClassName "Picker-component"]] $ pure $ renderDateTime fmtEnd end true]
  DurationEnd fmtDuration fmtEnd, DurationEnd duration end ->
    [ HH.div [HP.classes [HH.ClassName "Picker-component"]] $ pure $ renderDuration fmtDuration duration
    , HH.div [HP.classes [HH.ClassName "Picker-component"]] $ pure $ textElement { text: "/" }
    , HH.div [HP.classes [HH.ClassName "Picker-component"]] $ pure $ renderDateTime fmtEnd end false]
  StartDuration fmtStart fmtDuration, StartDuration start duration ->
    [ HH.div [HP.classes [HH.ClassName "Picker-component"]] $ pure $ renderDateTime fmtStart start false
    , HH.div [HP.classes [HH.ClassName "Picker-component"]] $ pure $ textElement { text: "/" }
    , HH.div [HP.classes [HH.ClassName "Picker-component"]] $ pure $ renderDuration fmtDuration duration]
  JustDuration fmtDuration, JustDuration duration ->
    [ HH.div [HP.classes [HH.ClassName "Picker-component"]] $ pure $ renderDuration fmtDuration duration]
  _ , _ -> [HH.text "can't render invalid value"]

renderDuration :: ∀ m. DurationF.Format -> IsoDuration -> HTML m
renderDuration fmt duration = HH.slot' cpDuration unit (Duration.picker fmt duration) unit (HE.input $ HandleDurationMessage)

renderDateTime :: ∀ m. DateTimeF.Format -> DateTime -> Boolean -> HTML m
renderDateTime fmt datetime idx = HH.slot' cpDateTime idx (DateTime.picker fmt datetime) unit (HE.input $ HandleDateTimeMessage idx)



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
      NotifyChange (Just newDuration) -> lmap (const newDuration) interval
      NotifyChange Nothing -> interval -- TODO wrap Interval in Maybe here too
  H.modify _{ interval = newInterval }
  H.raise (NotifyChange newInterval)
  pure next


toShape :: forall f a b. Bifunctor f => f a b -> f Unit Unit
toShape = bimap (const unit) (const unit)

evalPicker ∷ ∀ m . (PickerQuery (Maybe IntervalError) Input) ~> DSL m
evalPicker (SetValue interval next) = do
  {format} <- H.get
  if (toShape format /= toShape interval)
    then pure $ next $ Just IntervalIsNotInShapeOfFormat
    else do
      H.modify _{ interval = interval }
      case interval of
        StartEnd a b -> setDateTime false a *> setDateTime true b
        DurationEnd d a -> setDuration d *> setDateTime false a
        StartDuration a d -> setDateTime false a *> setDuration d
        JustDuration d -> setDuration d
      pure $ next Nothing
evalPicker (GetValue next) = H.gets _.interval <#> next

setDuration :: ∀ m. IsoDuration -> DSL m Unit
setDuration val = map mustBeMounted $ H.query' cpDuration unit $ H.request $ left <<< (SetValue (Just val))

setDateTime :: ∀ m. Boolean -> DateTime -> DSL m Unit
setDateTime idx val = map mustBeMounted $ H.query' cpDateTime idx $ H.request $ left <<< (SetValue val)
