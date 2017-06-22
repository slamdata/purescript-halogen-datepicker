module Halogen.Datepicker.Component.Interval where

import Prelude

import Data.Bifunctor (bimap, lmap)
import Data.DateTime (DateTime)
import Data.Either (Either(..), either)
import Data.Either.Nested (Either2)
import Data.Foldable (for_)
import Data.Functor.Coproduct (Coproduct, coproduct, right)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Interval (Interval(..), IsoDuration)
import Data.Maybe (Maybe(..), isNothing)
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.Datepicker.Component.DateTime as DateTime
import Halogen.Datepicker.Component.Duration (DurationError)
import Halogen.Datepicker.Component.Duration as Duration
import Halogen.Datepicker.Component.Types (BasePickerQuery(..), PickerMessage(..), PickerQuery(..), PickerValue, getValue, setValue, resetError)
import Halogen.Datepicker.Format.DateTime as DateTimeF
import Halogen.Datepicker.Format.Duration as DurationF
import Halogen.Datepicker.Format.Interval as F
import Halogen.Datepicker.Internal.Elements (textElement)
import Halogen.Datepicker.Internal.Utils (componentProps, transitionState, asLeft, mustBeMounted, pickerProps)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type State = PickerValue IntervalError IsoInterval
type IntervalError = Interval (Maybe DurationError) (Maybe DateTime.DateTimeError)
type IsoInterval = Interval IsoDuration DateTime

type Message = PickerMessage State

type Query = Coproduct QueryIn IntervalQuery
type QueryIn = PickerQuery (Maybe SetIntervalError) State
data SetIntervalError = IntervalIsNotInShapeOfFormat
data IntervalQuery a = Update MessageIn a
type MessageIn = Either Duration.Message (Tuple Boolean DateTime.Message)

type Slot = Either2 DurationSlot DateTimeSlot
type ChildQuery = Coproduct2 Duration.Query DateTime.Query
type DateTimeSlot = Boolean
type DurationSlot = Unit

cpDuration ∷ CP.ChildPath Duration.Query ChildQuery DurationSlot Slot
cpDuration = CP.cp1
cpDateTime ∷ CP.ChildPath DateTime.Query ChildQuery DateTimeSlot Slot
cpDateTime = CP.cp2

type HTML m = H.ParentHTML IntervalQuery ChildQuery Slot m
type DSL m = H.ParentDSL State Query ChildQuery Slot Message m


picker ∷ ∀ m. F.Format → H.Component HH.HTML Query Unit Message m
picker format = H.parentComponent
  { initialState: const Nothing
  , render: render format >>> bimap (map right) right
  , eval: coproduct (evalPicker format) (evalInterval format)
  , receiver: const Nothing
  }

render ∷ ∀ m. F.Format → State → HTML m
render format interval = HH.div (pickerProps interval) (renderCommand format)

renderCommand ∷ ∀ m. F.Format → Array (HTML m)
renderCommand format = map (HH.div componentProps <<< pure) case format of
  StartEnd fmtStart fmtEnd →
    [ renderDateTime fmtStart false
    , textElement { text: "/" }
    , renderDateTime fmtEnd true ]
  DurationEnd fmtDuration fmtEnd →
    [ renderDuration fmtDuration
    , textElement { text: "/" }
    , renderDateTime fmtEnd false ]
  StartDuration fmtStart fmtDuration →
    [ renderDateTime fmtStart false
    , textElement { text: "/" }
    , renderDuration fmtDuration ]
  JustDuration fmtDuration →
    [ renderDuration fmtDuration ]

renderDuration ∷ ∀ m. DurationF.Format → HTML m
renderDuration fmt = HH.slot' cpDuration unit (Duration.picker fmt) unit (HE.input $ Update <<< Left)

renderDateTime ∷ ∀ m. DateTimeF.Format → Boolean → HTML m
renderDateTime fmt idx = HH.slot' cpDateTime idx (DateTime.picker fmt) unit (HE.input $ Update <<< Right <<< (Tuple idx))


-- [1] - this case will not happen as interval will not be `Just Right`
--       if any of it's child is `Nothing` so return nonsence value
evalInterval ∷ ∀ m . F.Format → IntervalQuery ~> DSL m
evalInterval format (Update msg next) = do
  transitionState case _ of
    Nothing  → do
      newInterval ← buildInterval format
      case newInterval of
        Left (Tuple false _) → resetChildErrorBasedOnMessage msg
        _ → pure unit
      pure newInterval
    Just (Left err) → buildInterval format
    Just (Right prevInterval) → pure $ lmap (Tuple false) case msg of
      Left (NotifyChange newDuration) → case newDuration of
        Just (Left x) → Left $ bimap (const $ Just x) (const Nothing) format
        Nothing → Left $ bimap (const Nothing) (const Nothing) format -- [1]
        Just (Right duration) → Right $ lmap (const duration) prevInterval
      Right (Tuple idx (NotifyChange newDateTime)) → case newDateTime of
        Just (Left x) → Left $ bimap (const Nothing) (const $ Just x) format
        Nothing → Left $ bimap (const Nothing) (const Nothing) format -- [1]
        Just (Right dateTime) → Right case prevInterval of
          StartEnd a b → case idx of
            true → StartEnd dateTime b
            false → StartEnd a dateTime
          DurationEnd d a → DurationEnd d dateTime
          StartDuration a d → StartDuration dateTime d
          JustDuration d → JustDuration d
  pure next


buildInterval ∷ ∀ m. F.Format → DSL m (Either (Tuple Boolean IntervalError) IsoInterval)
buildInterval format = do
  vals ← collectValues format
  pure $ lmap addForce $ unVals vals

addForce ∷ IntervalError → Tuple Boolean IntervalError
addForce err = case err of
  StartEnd Nothing Nothing → Tuple false err
  DurationEnd Nothing Nothing → Tuple false err
  StartDuration Nothing Nothing → Tuple false err
  JustDuration Nothing → Tuple false err
  _ → Tuple true err

unVals ∷ Interval Duration.State DateTime.State → Either IntervalError IsoInterval
unVals vals = case bimap maybeLeft maybeLeft vals of
  StartEnd (Right dtStart) (Right dtEnd) → Right $ StartEnd dtStart dtEnd
  DurationEnd (Right dur) (Right dt) → Right $ DurationEnd dur dt
  StartDuration (Right dt) (Right dur) → Right $ StartDuration dt dur
  JustDuration (Right dur) → Right $ JustDuration dur
  interval → Left $ bimap toError toError interval

toError ∷ ∀ e a. Either (Maybe e) a → Maybe e
toError = asLeft >>> join

maybeLeft ∷ ∀ e a. Maybe (Either e a) → Either (Maybe e) a
maybeLeft (Just (Right a)) = Right a
maybeLeft (Just (Left a)) = Left $ Just a
maybeLeft Nothing = Left $ Nothing

collectValues ∷ ∀ d a m
  . Interval d a
  → DSL m (Interval Duration.State DateTime.State)
collectValues format = case format of
  StartEnd a b → StartEnd <$> getDateTime false <*> getDateTime true
  DurationEnd d a → DurationEnd <$> getDuration <*> getDateTime false
  StartDuration a d → StartDuration <$> getDateTime false <*> getDuration
  JustDuration d → JustDuration <$> getDuration

resetChildErrorBasedOnMessage ∷ ∀ m. MessageIn → DSL m Unit
resetChildErrorBasedOnMessage (Left (NotifyChange (Just (Left _)))) = resetDuration
resetChildErrorBasedOnMessage (Right (Tuple idx (NotifyChange (Just (Left _))))) = resetDateTime idx
resetChildErrorBasedOnMessage _ = pure unit

resetChildError ∷ ∀ m. F.Format → DSL m Unit
resetChildError format = do
  onFormat resetDateTime resetDuration format

onFormat ∷ ∀ m a d
  . Apply m
  ⇒ (Boolean → m Unit)
  → m Unit
  → Interval d a
  → m Unit
onFormat onDateTime onDuration format = case format of
  StartEnd a b → onDateTime false *> onDateTime true
  DurationEnd d a → onDuration *> onDateTime false
  StartDuration a d → onDateTime false *> onDuration
  JustDuration d → onDuration

evalPicker ∷ ∀ m. F.Format → QueryIn ~> DSL m
evalPicker format (ResetError next) = do
  H.put Nothing
  resetChildError format
  pure next
evalPicker format (Base (SetValue interval reply)) = do
  res ← case viewInterval format interval <#> setInterval of
    Just x → x $> Nothing
    Nothing → pure $ Just IntervalIsNotInShapeOfFormat
  when (isNothing res) $ H.put interval
  pure $ reply res
evalPicker _ (Base (GetValue reply)) = H.get <#> reply

type ChildStates
  = Interval (Maybe Duration.State) (Maybe DateTime.State)

setInterval ∷ ∀ m. ChildStates → DSL m Unit
setInterval = case _ of
  StartEnd a b → do
    for_ a $ setDateTime false
    for_ b $ setDateTime true
  DurationEnd d a → do
    for_ d setDuration
    for_ a $ setDateTime false
  StartDuration a d → do
    for_ a $ setDateTime false
    for_ d setDuration
  JustDuration d → do
    for_ d setDuration

viewInterval ∷ F.Format → State → Maybe ChildStates
viewInterval format input = case format, mapedState input of
  StartEnd _ _ , Just interval@(StartEnd _ _) → Just $ interval
  DurationEnd _ _ , Just interval@(DurationEnd _ _) → Just $ interval
  StartDuration _ _ , Just interval@(StartDuration _ _) → Just $ interval
  JustDuration _ , Just interval@(JustDuration _) → Just $ interval
  _, Nothing → Just $ bimap (const $ Just Nothing) (const $ Just Nothing) format
  _ , _ → Nothing
  where
  mapedState ∷ State → Maybe ChildStates
  mapedState = map $ either (bimap mkErr mkErr) (bimap mkVal mkVal)
  mkVal ∷ ∀ e a. a → Maybe (PickerValue e a)
  mkVal = Just <<< Just <<< Right
  mkErr ∷ ∀ e a. Maybe e → Maybe (PickerValue e a)
  mkErr = map (Just <<< Left)

getDuration ∷ ∀ m. DSL m Duration.State
getDuration = queryDuration $ getValue

getDateTime ∷ ∀ m. Boolean → DSL m DateTime.State
getDateTime idx  = queryDateTime idx $ getValue

setDuration ∷ ∀ m. Duration.State → DSL m Unit
setDuration val = queryDuration $ setValue val

setDateTime ∷ ∀ m. Boolean → DateTime.State → DSL m Unit
setDateTime idx val = queryDateTime idx $ setValue val

resetDuration ∷ ∀ m. DSL m Unit
resetDuration = queryDuration $ resetError

resetDateTime ∷ ∀ m. Boolean → DSL m Unit
resetDateTime idx = queryDateTime idx $ resetError

queryDuration ∷ ∀ m a. Duration.Query a → DSL m a
queryDuration q = H.query' cpDuration unit q >>= mustBeMounted

queryDateTime ∷ ∀ m a. DateTimeSlot → DateTime.Query a → DSL m a
queryDateTime idx q = H.query' cpDateTime idx q >>= mustBeMounted
