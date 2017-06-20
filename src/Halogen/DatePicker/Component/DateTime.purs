module Halogen.Datepicker.Component.DateTime where

import Prelude

import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Bifunctor (bimap, lmap)
import Data.Date (Date)
import Data.DateTime (DateTime, date, modifyDate, modifyTime, time)
import Data.Either (Either(..))
import Data.Either.Nested (Either2)
import Data.Foldable (class Foldable, fold, length)
import Data.Functor.Coproduct (Coproduct, coproduct, right, left)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Maybe (Maybe(..))
import Data.Maybe.Last (Last(..))
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.Profunctor.Join (Join(..))
import Data.Profunctor.Star (Star(..))
import Data.Time (Time)
import Data.Traversable (for, for_)
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.Datepicker.Component.Date (DateError)
import Halogen.Datepicker.Component.Date as Date
import Halogen.Datepicker.Component.Time (TimeError)
import Halogen.Datepicker.Component.Time as Time
import Halogen.Datepicker.Component.Types (BasePickerQuery(..), PickerMessage(..), PickerQuery(..), PickerValue, value)
import Halogen.Datepicker.Format.DateTime as F
import Halogen.Datepicker.Internal.Utils (steper, pickerClasses, mustBeMounted)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type MessageIn = Either Date.Message Time.Message
data DateTimeQuery a = Update MessageIn a

type DateTimeError = Tuple (Maybe DateError) (Maybe TimeError)
type DateTimeErrorLast = Tuple (Last DateError) (Last TimeError)
type Input = PickerValue DateTimeError DateTime
type QueryIn = PickerQuery Unit Input
type Query = Coproduct QueryIn DateTimeQuery
type Message = PickerMessage Input

type State =
  { format ∷ F.Format
  , dateTime ∷ Input
  }

type ChildQuery = Coproduct2 Date.Query Time.Query
type Slot = Either2 Unit Unit
cpDate ∷ CP.ChildPath Date.Query ChildQuery Unit Slot
cpDate = CP.cp1
cpTime ∷ CP.ChildPath Time.Query ChildQuery Unit Slot
cpTime = CP.cp2


type HTML m = H.ParentHTML DateTimeQuery ChildQuery Slot m
type DSL m = H.ParentDSL State Query ChildQuery Slot Message m


picker ∷ ∀ m. F.Format → H.Component HH.HTML Query Unit Message m
picker format = H.parentComponent
  { initialState: const $ {format, dateTime: Nothing}
  , render: render >>> bimap (map right) right
  , eval: coproduct evalPicker evalDateTime
  , receiver: const Nothing
  }

render ∷ ∀ m. State → HTML m
render s = HH.div [ HP.classes $ pickerClasses s.dateTime ]
  (f <$> unwrap s.format)
  where
  f a = HH.div [HP.classes [HH.ClassName "Picker-component"]] $ pure $ renderCommand a

renderCommand ∷ ∀ m. F.Command → HTML m
renderCommand cmd = case cmd of 
  F.Time fmt → HH.slot' cpTime unit (Time.picker fmt) unit (HE.input $ Right >>> Update)
  F.Date fmt → HH.slot' cpDate unit (Date.picker fmt) unit (HE.input $ Left >>> Update)


evalDateTime ∷ ∀ m . DateTimeQuery ~> DSL m
evalDateTime (Update msg next) = do
  s <- H.get
  nextDateTime <- map (steper s.dateTime) case s.dateTime of
    Nothing → do
      dt <- buildDateTime
      case dt of
        Left (Tuple false _) → resetChildErrorBasedOnMessage msg
        _ → pure unit
      pure dt
    Just (Left err)  → buildDateTime
    Just (Right dt) → pure $ lmap (Tuple false) case msg of
      Left  (NotifyChange newDate) → case newDate of
        Just (Right date) → Right $ setDateDt date dt
        Just (Left x) → Left $ dateError x
        Nothing → Left $ Tuple Nothing Nothing
      Right (NotifyChange newTime) → case newTime of
        Just (Right time) → Right $ setTimeDt time dt
        Just (Left x) → Left $ timeError x
        Nothing → Left $ Tuple Nothing Nothing
  H.modify _{ dateTime = nextDateTime }
  unless (nextDateTime == s.dateTime) $ H.raise (NotifyChange nextDateTime)
  pure next

resetChildErrorBasedOnMessage ∷ ∀ m. MessageIn → DSL m Unit
resetChildErrorBasedOnMessage (Left (NotifyChange (Just (Left _)))) = resetDate
resetChildErrorBasedOnMessage (Right (NotifyChange (Just (Left _)))) = resetTime
resetChildErrorBasedOnMessage _ = pure unit

resetChildError ∷ ∀ m. DSL m Unit
resetChildError = do
  {format} <- H.get
  for_ (unwrap format) case _ of
    F.Time _ → resetTime
    F.Date _ → resetDate


timeError ∷ TimeError → DateTimeError
timeError x = Tuple Nothing (Just x)

dateError ∷ DateError → DateTimeError
dateError x = Tuple (Just x) Nothing

type StepM = Join (Star (Writer (Maybe (Tuple (Additive Int) DateTimeErrorLast)))) DateTime
formatToSteps ∷ ∀ m. F.Format → DSL m (Array StepM)
formatToSteps format = for (unwrap format) case _ of
  F.Time _ → applyTime <$> queryTime (H.request $ left <<< Base <<< GetValue)
  F.Date _ → applyDate <$> queryDate (H.request $ left <<< Base <<< GetValue)
  where
  applyTime ∷ PickerValue TimeError Time → StepM
  applyTime val = Join $ Star $ \dt → case val of
    Just (Right time) → pure $ setTimeDt time dt
    Just (Left err) → tell (Just $ Tuple (Additive 1) $ bimap Last Last $ timeError err) *> pure dt
    Nothing → tell (Just $ Tuple (Additive 0) $ Tuple (Last Nothing) (Last Nothing)) *> pure dt
  applyDate ∷ PickerValue DateError Date → StepM
  applyDate val = Join $ Star $ \dt → case val of
    Just (Right date) → pure $ setDateDt date dt
    Just (Left err) → tell (Just $ Tuple (Additive 1) $ bimap Last Last $ dateError err) *> pure dt
    Nothing → tell (Just $ Tuple (Additive 0) $ Tuple (Last Nothing) (Last Nothing)) *> pure dt

setTimeDt ∷ Time → DateTime → DateTime
setTimeDt x dt = modifyTime (const x) dt
setDateDt ∷ Date → DateTime → DateTime
setDateDt x dt = modifyDate (const x) dt


stepsToFunc ∷ ∀ f. Foldable f => Int → f StepM → DateTime → Either (Tuple Boolean DateTimeError) DateTime
stepsToFunc childCount steps dt = fold steps # \(Join (Star f)) → case runWriter $ f dt of
  Tuple res Nothing → Right res
  Tuple res (Just (Tuple (Additive errCount) err)) → Left $ Tuple
    (errCount > 0  && errCount < childCount) -- if we hit arror 0 or childCount we shuoldn't force
    (bimap unwrap unwrap err)


buildDateTime ∷ ∀ m. DSL m (Either (Tuple Boolean DateTimeError) DateTime)
buildDateTime = do
  {format} <- H.get
  steps <- formatToSteps format
  pure $ stepsToFunc (length $ unwrap format) steps bottom


evalPicker ∷ ∀ m . QueryIn ~> DSL m
evalPicker (ResetError next) = do
  H.modify _{ dateTime = Nothing }
  resetChildError
  pure next
evalPicker (Base (SetValue dateTime next)) = do
  H.modify _{ dateTime = dateTime }
  {format} <- H.get
  for_ (unwrap format) case _ of
    F.Time _ → setTime $ value dateTime <#> (time >>> Right)
    F.Date _ → setDate $ value dateTime <#> (date >>> Right)
  pure $ next unit
evalPicker (Base (GetValue next)) = H.gets _.dateTime <#> next

setTime ∷ ∀ m. PickerValue TimeError Time → DSL m Unit
setTime val = queryTime $ H.request $ left <<< (Base <<< SetValue val)

setDate ∷ ∀ m. PickerValue DateError Date → DSL m Unit
setDate val = queryDate $ H.request $ left <<< (Base <<< SetValue val)

resetTime ∷ ∀ m. DSL m Unit
resetTime = queryTime $ H.action $ left <<< ResetError

resetDate ∷ ∀ m. DSL m Unit
resetDate = queryDate $ H.action $ left <<< ResetError

queryTime ∷ ∀ m a. Time.Query a → DSL m a
queryTime q = map mustBeMounted $ H.query' cpTime unit $ q

queryDate ∷ ∀ m a. Date.Query a → DSL m a
queryDate q = map mustBeMounted $ H.query' cpDate unit $ q
