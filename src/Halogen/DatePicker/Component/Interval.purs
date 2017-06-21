module Halogen.Datepicker.Component.Interval where

import Prelude

import Data.Bifunctor (bimap, lmap)
import Data.DateTime (DateTime)
import Data.Either (Either(..), either)
import Data.Either.Nested (Either2)
import Data.Foldable (for_)
import Data.Functor.Coproduct (Coproduct, coproduct, right, left)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Interval (Interval(..), IsoDuration)
import Data.Maybe (Maybe(..), isNothing)
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.Datepicker.Component.DateTime (DateTimeError)
import Halogen.Datepicker.Component.DateTime as DateTime
import Halogen.Datepicker.Component.Duration (DurationError)
import Halogen.Datepicker.Component.Duration as Duration
import Halogen.Datepicker.Component.Types (BasePickerQuery(..), PickerMessage(..), PickerQuery(..), PickerValue)
import Halogen.Datepicker.Format.DateTime as DateTimeF
import Halogen.Datepicker.Format.Duration as DurationF
import Halogen.Datepicker.Format.Interval as F
import Halogen.Datepicker.Internal.Elements (textElement)
import Halogen.Datepicker.Internal.Utils (asLeft, mustBeMounted, pickerClasses, steper)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type IntervalError = Interval (Maybe DurationError) (Maybe DateTimeError)
type IsoInterval = Interval IsoDuration DateTime
type Input = PickerValue IntervalError IsoInterval
data SetIntervalError = IntervalIsNotInShapeOfFormat

type MessageIn = Either Duration.Message (Tuple Boolean DateTime.Message)
data IntervalQuery a = Update MessageIn  a

type Query = Coproduct (PickerQuery (Maybe SetIntervalError) Input) IntervalQuery
type Message = PickerMessage Input
type State =
  { format ∷ F.Format
  , interval ∷ Input
  }

type ChildQuery = Coproduct2 Duration.Query DateTime.Query
type Slot = Either2 Unit Boolean

cpDuration ∷ CP.ChildPath Duration.Query ChildQuery Unit Slot
cpDuration = CP.cp1
cpDateTime ∷ CP.ChildPath DateTime.Query ChildQuery Boolean Slot
cpDateTime = CP.cp2


type HTML m = H.ParentHTML IntervalQuery ChildQuery Slot m
type DSL m = H.ParentDSL State Query ChildQuery Slot Message m


picker ∷ ∀ m. F.Format → H.Component HH.HTML Query Unit Message m
picker format = H.parentComponent
  { initialState: const $ {format, interval: Nothing}
  , render: render >>> bimap (map right) right
  , eval: coproduct evalPicker evalInterval
  , receiver: const Nothing
  }

render ∷ ∀ m. State → HTML m
render s = HH.div [HP.classes $ pickerClasses s.interval] case s.format of
  StartEnd fmtStart fmtEnd → map elem
    [ renderDateTime fmtStart false
    , textElement { text: "/" }
    , renderDateTime fmtEnd true ]
  DurationEnd fmtDuration fmtEnd → map elem
    [ renderDuration fmtDuration
    , textElement { text: "/" }
    , renderDateTime fmtEnd false ]
  StartDuration fmtStart fmtDuration → map elem
    [ renderDateTime fmtStart false
    , textElement { text: "/" }
    , renderDuration fmtDuration ]
  JustDuration fmtDuration → map elem
    [ renderDuration fmtDuration ]
  where
  elem a = HH.div [HP.classes [HH.ClassName "Picker-component"]] $ pure $ a

renderDuration ∷ ∀ m. DurationF.Format → HTML m
renderDuration fmt = HH.slot' cpDuration unit (Duration.picker fmt) unit (HE.input $ Update <<< Left)

renderDateTime ∷ ∀ m. DateTimeF.Format → Boolean → HTML m
renderDateTime fmt idx = HH.slot' cpDateTime idx (DateTime.picker fmt) unit (HE.input $ Update <<< Right <<< (Tuple idx))


-- [1] - this case will not happen as interval will not be `Just Right`
--       if any of it's child is `Nothing` so return nonsence value
evalInterval ∷ ∀ m . IntervalQuery ~> DSL m
evalInterval (Update msg next) = do
  s <- H.get
  nextInterval <- map (steper s.interval) case s.interval of
    Nothing  → do
      interval <- buildInterval
      case interval of
        Left (Tuple false _) → resetChildErrorBasedOnMessage msg
        _ → pure unit
      pure interval
    Just (Left err) → buildInterval
    Just (Right interval) → pure $ lmap (Tuple false) case msg of
      Left (NotifyChange newDuration) → case newDuration of
        Just (Left x) → Left $ bimap (const $ Just x) (const Nothing) s.format
        Nothing → Left $ bimap (const Nothing) (const Nothing) s.format -- [1]
        Just (Right duration) → Right $ lmap (const duration) interval
      Right (Tuple idx (NotifyChange newDateTime)) → case newDateTime of
        Just (Left x) → Left $ bimap (const Nothing) (const $ Just x) s.format
        Nothing → Left $ bimap (const Nothing) (const Nothing) s.format -- [1]
        Just (Right dateTime) → Right case interval of
          StartEnd a b → case idx of
            true → StartEnd dateTime b
            false → StartEnd a dateTime
          DurationEnd d a → DurationEnd d dateTime
          StartDuration a d → StartDuration dateTime d
          JustDuration d → JustDuration d
  H.modify _{ interval = nextInterval }
  unless (nextInterval == s.interval) $ H.raise (NotifyChange nextInterval)
  pure next


buildInterval ∷ ∀ m. DSL m (Either (Tuple Boolean IntervalError) IsoInterval)
buildInterval = do
  {format} <- H.get
  vals <- collectValues format
  pure $ lmap addForce $ unVals vals

addForce ∷ IntervalError → Tuple Boolean IntervalError
addForce err = case err of
  StartEnd Nothing Nothing → Tuple false err
  DurationEnd Nothing Nothing → Tuple false err
  StartDuration Nothing Nothing → Tuple false err
  JustDuration Nothing → Tuple false err
  _ → Tuple true err

unVals ∷ Interval Duration.Input DateTime.Input → Either IntervalError IsoInterval
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

collectValues ∷ ∀ d a m. Interval d a → DSL m (Interval Duration.Input DateTime.Input)
collectValues format = case format of
  StartEnd a b → StartEnd <$> getDateTime false <*> getDateTime true
  DurationEnd d a → DurationEnd <$> getDuration <*> getDateTime false
  StartDuration a d → StartDuration <$> getDateTime false <*> getDuration
  JustDuration d → JustDuration <$> getDuration

getDuration ∷ ∀ m. DSL m (PickerValue DurationError IsoDuration)
getDuration = queryDuration $ H.request $ left <<< Base <<< GetValue

getDateTime ∷ ∀ m. Boolean → DSL m (PickerValue DateTimeError DateTime)
getDateTime idx  = queryDateTime idx $ H.request $ left <<< Base <<< GetValue

resetChildErrorBasedOnMessage ∷ ∀ m. MessageIn → DSL m Unit
resetChildErrorBasedOnMessage (Left (NotifyChange (Just (Left _)))) = resetDuration
resetChildErrorBasedOnMessage (Right (Tuple idx (NotifyChange (Just (Left _))))) = resetDateTime idx
resetChildErrorBasedOnMessage _ = pure unit

resetChildError ∷ ∀ m. DSL m Unit
resetChildError = do
  {format} <- H.get
  onFormat resetDateTime resetDuration format

onFormat ∷ ∀ m a d. Apply m => (Boolean → m Unit) → m Unit → Interval d a → m Unit
onFormat onDateTime onDuration format = case format of
  StartEnd a b → onDateTime false *> onDateTime true
  DurationEnd d a → onDuration *> onDateTime false
  StartDuration a d → onDateTime false *> onDuration
  JustDuration d → onDuration

evalPicker ∷ ∀ m . (PickerQuery (Maybe SetIntervalError) Input) ~> DSL m
evalPicker (ResetError next) = do
  H.modify _{ interval = Nothing }
  resetChildError
  pure next
evalPicker (Base (SetValue interval next)) = do
  {format} <- H.get
  res <- case viewInterval format interval of
    Just (StartEnd a b) → for_ a (setDateTime false) *> for_ b (setDateTime true) $> Nothing
    Just (DurationEnd d a) → for_ d setDuration *> for_ a (setDateTime false) $> Nothing
    Just (StartDuration a d) → for_ a (setDateTime false) *> for_ d setDuration $> Nothing
    Just (JustDuration d) → for_ d setDuration $> Nothing
    Nothing → pure $ Just IntervalIsNotInShapeOfFormat
  when (isNothing res) $ H.modify _{ interval = interval }
  pure $ next res
evalPicker (Base (GetValue next)) = H.gets _.interval <#> next

viewInterval ∷ F.Format → Input → Maybe (Interval (Maybe Duration.Input) (Maybe DateTime.Input))
viewInterval format input = case format, mapedInput input of
  StartEnd _ _ , Just interval@(StartEnd _ _) → Just $ interval
  DurationEnd _ _ , Just interval@(DurationEnd _ _) → Just $ interval
  StartDuration _ _ , Just interval@(StartDuration _ _) → Just $ interval
  JustDuration _ , Just interval@(JustDuration _) → Just $ interval
  _, Nothing → Just $ bimap (const $ Just Nothing) (const $ Just Nothing) format
  _ , _ → Nothing
  where
  mapedInput ∷ Input → Maybe (Interval (Maybe Duration.Input) (Maybe DateTime.Input))
  mapedInput = map $ either (bimap mkErr mkErr) (bimap mkVal mkVal)
  mkVal ∷ ∀ e a. a → Maybe (PickerValue e a)
  mkVal = Just <<< Just <<< Right
  mkErr ∷ ∀ e a. Maybe e → Maybe (PickerValue e a)
  mkErr = map (Just <<< Left)

setDuration ∷ ∀ m. PickerValue DurationError IsoDuration → DSL m Unit
setDuration val = queryDuration $ H.request $ left <<< (Base <<< SetValue val)

setDateTime ∷ ∀ m. Boolean → PickerValue DateTimeError DateTime → DSL m Unit
setDateTime idx val = queryDateTime idx $ H.request $ left <<< (Base <<< SetValue val)

resetDuration ∷ ∀ m. DSL m Unit
resetDuration = queryDuration $ H.action $ left <<< ResetError

resetDateTime ∷ ∀ m. Boolean → DSL m Unit
resetDateTime idx = queryDateTime idx $ H.action $ left <<< ResetError

queryDuration ∷ ∀ m a. Duration.Query a → DSL m a
queryDuration q = map mustBeMounted $ H.query' cpDuration unit $ q

queryDateTime ∷ ∀ m a. Boolean → DateTime.Query a → DSL m a
queryDateTime idx q = map mustBeMounted $ H.query' cpDateTime idx $ q
