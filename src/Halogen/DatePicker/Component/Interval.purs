module Halogen.Datepicker.Component.Interval where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Data.Bifunctor (bimap, lmap)
import Data.DateTime (DateTime)
import Data.Either (Either(..), either)
import Data.Foldable (for_)
import Data.Interval (Interval(..))
import Data.Interval.Duration.Iso (IsoDuration)
import Data.Maybe (Maybe(..), isNothing)
import Data.Tuple (Tuple(..))
import Effect.Exception as Ex
import Halogen as H
import Halogen.Datepicker.Component.DateTime as DateTime
import Halogen.Datepicker.Component.Duration (DurationError)
import Halogen.Datepicker.Component.Duration as Duration
import Halogen.Datepicker.Component.Types (BasePickerQuery(..), PickerQuery(..), PickerValue, getValue, setValue, resetError)
import Halogen.Datepicker.Config (Config, defaultConfig)
import Halogen.Datepicker.Format.DateTime as DateTimeF
import Halogen.Datepicker.Format.Duration as DurationF
import Halogen.Datepicker.Format.Interval as F
import Halogen.Datepicker.Internal.Elements (textElement)
import Halogen.Datepicker.Internal.Utils (asLeft, componentProps, mustBeMounted, pickerProps, transitionState)
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))

type State = PickerValue IntervalError IsoInterval
type IntervalError = Interval (Maybe DurationError) (Maybe DateTime.DateTimeError)
type IsoInterval = Interval IsoDuration DateTime

type Message = PickerValue IntervalError IsoInterval

type Query = PickerQuery (Maybe SetIntervalError) State
data SetIntervalError = IntervalIsNotInShapeOfFormat
type MessageIn = Either Duration.Message (Tuple Boolean DateTime.Message)

type Slots =
  ( dateTime ∷ DateTime.Slot Boolean
  , duration ∷ Duration.Slot Unit
  )

_dateTime = Proxy ∷ Proxy "dateTime"
_duration = Proxy ∷ Proxy "duration"

type Slot = H.Slot Query Message

type HTML m = H.ComponentHTML MessageIn Slots m
type DSL = H.HalogenM State MessageIn Slots Message

picker ∷ ∀ m. MonadError Ex.Error m ⇒ F.Format → H.Component Query Unit Message m
picker = pickerWithConfig defaultConfig

pickerWithConfig
  ∷ ∀ m
  . MonadError Ex.Error m
  ⇒ Config
  → F.Format
  → H.Component Query Unit Message m
pickerWithConfig config format =
  H.mkComponent
    { initialState: const Nothing
    , render: render config format
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction format
        , handleQuery = handleQuery format
        }
    }

render ∷ ∀ m. MonadError Ex.Error m ⇒ Config → F.Format → State → HTML m
render config format interval = HH.div (pickerProps config interval) (renderCommand config format)

renderCommand ∷ ∀ m. MonadError Ex.Error m ⇒ Config → F.Format → Array (HTML m)
renderCommand config format = map (HH.div (componentProps config) <<< pure) case format of
  StartEnd fmtStart fmtEnd →
    [ renderDateTime config fmtStart false
    , textElement config { text: "/" }
    , renderDateTime config fmtEnd true ]
  DurationEnd fmtDuration fmtEnd →
    [ renderDuration config fmtDuration
    , textElement config { text: "/" }
    , renderDateTime config fmtEnd false ]
  StartDuration fmtStart fmtDuration →
    [ renderDateTime config fmtStart false
    , textElement config { text: "/" }
    , renderDuration config fmtDuration ]
  DurationOnly fmtDuration →
    [ renderDuration config fmtDuration ]

renderDuration ∷ ∀ m. MonadError Ex.Error m ⇒ Config → DurationF.Format → HTML m
renderDuration config fmt =
  HH.slot
    _duration
    unit
    (Duration.pickerWithConfig config fmt)
    unit
    Left

renderDateTime ∷ ∀ m. MonadError Ex.Error m ⇒ Config → DateTimeF.Format → Boolean → HTML m
renderDateTime config fmt idx =
  HH.slot
    _dateTime
    idx
    (DateTime.pickerWithConfig config fmt)
    unit
    (\val → Right (Tuple idx val))

-- [1] - this case will not happen as interval will not be `Just Right`
--       if any of it's child is `Nothing` so return nonsence value
handleAction ∷ ∀ m. MonadError Ex.Error m ⇒ F.Format → MessageIn → DSL m Unit
handleAction format msg = do
  transitionState case _ of
    Nothing  → do
      newInterval ← buildInterval format
      case newInterval of
        Left (Tuple false _) → resetChildErrorBasedOnMessage msg
        _ → pure unit
      pure newInterval
    Just (Left _) → buildInterval format
    Just (Right prevInterval) → pure $ lmap (Tuple false) case msg of
      Left newDuration → case newDuration of
        Just (Left x) → Left $ bimap (const $ Just x) (const Nothing) format
        Nothing → Left $ bimap (const Nothing) (const Nothing) format -- [1]
        Just (Right duration) → Right $ lmap (const duration) prevInterval
      Right (Tuple idx newDateTime) → case newDateTime of
        Just (Left x) → Left $ bimap (const Nothing) (const $ Just x) format
        Nothing → Left $ bimap (const Nothing) (const Nothing) format -- [1]
        Just (Right dateTime) → Right case prevInterval of
          StartEnd a b → case idx of
            true → StartEnd dateTime b
            false → StartEnd a dateTime
          DurationEnd d _ → DurationEnd d dateTime
          StartDuration _ d → StartDuration dateTime d
          DurationOnly d → DurationOnly d

buildInterval ∷ ∀ m. MonadError Ex.Error m ⇒ F.Format → DSL m (Either (Tuple Boolean IntervalError) IsoInterval)
buildInterval format = do
  vals ← collectValues format
  pure $ lmap addForce $ unVals vals

addForce ∷ IntervalError → Tuple Boolean IntervalError
addForce err = case err of
  StartEnd Nothing Nothing → Tuple false err
  DurationEnd Nothing Nothing → Tuple false err
  StartDuration Nothing Nothing → Tuple false err
  DurationOnly Nothing → Tuple false err
  _ → Tuple true err

unVals ∷ Interval Duration.State DateTime.State → Either IntervalError IsoInterval
unVals vals = case bimap maybeLeft maybeLeft vals of
  StartEnd (Right dtStart) (Right dtEnd) → Right $ StartEnd dtStart dtEnd
  DurationEnd (Right dur) (Right dt) → Right $ DurationEnd dur dt
  StartDuration (Right dt) (Right dur) → Right $ StartDuration dt dur
  DurationOnly (Right dur) → Right $ DurationOnly dur
  interval → Left $ bimap toError toError interval

toError ∷ ∀ e a. Either (Maybe e) a → Maybe e
toError = asLeft >>> join

maybeLeft ∷ ∀ e a. Maybe (Either e a) → Either (Maybe e) a
maybeLeft (Just (Right a)) = Right a
maybeLeft (Just (Left a)) = Left $ Just a
maybeLeft Nothing = Left $ Nothing

collectValues
  ∷ ∀ d a m
  . MonadError Ex.Error m
  ⇒ Interval d a
  → DSL m (Interval Duration.State DateTime.State)
collectValues format = case format of
  StartEnd _ _ → StartEnd <$> getDateTime false <*> getDateTime true
  DurationEnd _ _ → DurationEnd <$> getDuration <*> getDateTime false
  StartDuration _ _ → StartDuration <$> getDateTime false <*> getDuration
  DurationOnly _ → DurationOnly <$> getDuration

resetChildErrorBasedOnMessage ∷ ∀ m. MonadError Ex.Error m ⇒ MessageIn → DSL m Unit
resetChildErrorBasedOnMessage (Left (Just (Left _))) = resetDuration
resetChildErrorBasedOnMessage (Right (Tuple idx (Just (Left _)))) = resetDateTime idx
resetChildErrorBasedOnMessage _ = pure unit

resetChildError ∷ ∀ m. MonadError Ex.Error m ⇒ F.Format → DSL m Unit
resetChildError format = do
  onFormat resetDateTime resetDuration format

onFormat
  ∷ ∀ m a d
  . Apply m
  ⇒ (Boolean → m Unit)
  → m Unit
  → Interval d a
  → m Unit
onFormat onDateTime onDuration format = case format of
  StartEnd _ _ → onDateTime false *> onDateTime true
  DurationEnd _ _ → onDuration *> onDateTime false
  StartDuration _ _ → onDateTime false *> onDuration
  DurationOnly _ → onDuration

handleQuery ∷ ∀ m a. MonadError Ex.Error m ⇒ F.Format → Query a → DSL m (Maybe a)
handleQuery format = case _ of
  ResetError a → do
    H.put Nothing
    resetChildError format
    pure $ Just a
  Base (SetValue interval k) → do
    res ← case viewInterval format interval <#> setInterval of
      Just x → x $> Nothing
      Nothing → pure $ Just IntervalIsNotInShapeOfFormat
    when (isNothing res) $ H.put interval
    pure $ Just $ k res
  Base (GetValue k) →
    Just <<< k <$> H.get

type ChildStates
  = Interval (Maybe Duration.State) (Maybe DateTime.State)

setInterval ∷ ∀ m. MonadError Ex.Error m ⇒ ChildStates → DSL m Unit
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
  DurationOnly d → do
    for_ d setDuration

viewInterval ∷ F.Format → State → Maybe ChildStates
viewInterval format input = case format, mappedState input of
  StartEnd _ _ , Just interval@(StartEnd _ _) → Just interval
  DurationEnd _ _ , Just interval@(DurationEnd _ _) → Just interval
  StartDuration _ _ , Just interval@(StartDuration _ _) → Just interval
  DurationOnly _ , Just interval@(DurationOnly _) → Just interval
  _, Nothing → Just $ bimap (const $ Just Nothing) (const $ Just Nothing) format
  _ , _ → Nothing
  where
  mappedState ∷ State → Maybe ChildStates
  mappedState = map $ either (bimap mkErr mkErr) (bimap mkVal mkVal)
  mkVal ∷ ∀ e a. a → Maybe (PickerValue e a)
  mkVal = Just <<< Just <<< Right
  mkErr ∷ ∀ e a. Maybe e → Maybe (PickerValue e a)
  mkErr = map (Just <<< Left)

getDuration ∷ ∀ m. MonadError Ex.Error m ⇒ DSL m Duration.State
getDuration = queryDuration $ getValue

getDateTime ∷ ∀ m. MonadError Ex.Error m ⇒ Boolean → DSL m DateTime.State
getDateTime idx = queryDateTime idx getValue

setDuration ∷ ∀ m. MonadError Ex.Error m ⇒ Duration.State → DSL m Unit
setDuration val = queryDuration $ setValue val

setDateTime ∷ ∀ m. MonadError Ex.Error m ⇒ Boolean → DateTime.State → DSL m Unit
setDateTime idx val = queryDateTime idx $ setValue val

resetDuration ∷ ∀ m. MonadError Ex.Error m ⇒ DSL m Unit
resetDuration = queryDuration $ resetError

resetDateTime ∷ ∀ m. MonadError Ex.Error m ⇒ Boolean → DSL m Unit
resetDateTime idx = queryDateTime idx resetError

queryDuration ∷ ∀ m a. MonadError Ex.Error m ⇒ Duration.Query a → DSL m a
queryDuration q = H.query _duration unit q >>= mustBeMounted

queryDateTime ∷ ∀ m a. MonadError Ex.Error m ⇒ Boolean → DateTime.Query a → DSL m a
queryDateTime idx q = H.query _dateTime idx q >>= mustBeMounted
