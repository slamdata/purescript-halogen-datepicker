module Halogen.Datepicker.Component.DateTime where

import Prelude

import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Array (sort)
import Data.Bifunctor (bimap, lmap)
import Data.Date (Date)
import Data.DateTime (DateTime, date, modifyDate, modifyTime, time)
import Data.Either (Either(..))
import Data.Either.Nested (Either2)
import Data.Foldable (length)
import Data.Functor.Coproduct (Coproduct, coproduct, right)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe.Last (Last(..))
import Data.Monoid (mempty)
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
import Halogen.Datepicker.Component.Types (BasePickerQuery(..), PickerMessage(..), PickerQuery(..), PickerValue, value, getValue, setValue, resetError)
import Halogen.Datepicker.Format.DateTime as F
import Halogen.Datepicker.Internal.Utils (componentProps, foldSteps, mustBeMounted, pickerProps, transitionState)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type State = PickerValue DateTimeError DateTime
type DateTimeError = DateTimeErrorF Maybe
type DateTimeErrorF f = Tuple (f DateError) (f TimeError)
type DateTimeErrorLast = DateTimeErrorF Last

type Message = PickerMessage State

type Query = Coproduct QueryIn DateTimeQuery
type QueryIn = PickerQuery Unit State
data DateTimeQuery a = Update MessageIn a
type MessageIn = Either Date.Message Time.Message

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
  { initialState: const Nothing
  , render: render format >>> bimap (map right) right
  , eval: coproduct (evalPicker format) (evalDateTime format)
  , receiver: const Nothing
  }

render ∷ ∀ m. F.Format → State → HTML m
render format dateTime = HH.div (pickerProps dateTime) (unwrap format <#> renderCommand)

renderCommand ∷ ∀ m. F.Command → HTML m
renderCommand cmd = HH.div componentProps $ pure case cmd of
  F.Time fmt → HH.slot' cpTime unit (Time.picker fmt) unit (HE.input $ Right >>> Update)
  F.Date fmt → HH.slot' cpDate unit (Date.picker fmt) unit (HE.input $ Left >>> Update)


evalDateTime ∷ ∀ m . F.Format → DateTimeQuery ~> DSL m
evalDateTime format (Update msg next) = do
  transitionState case _ of
    Nothing → do
      dt ← buildDateTime format
      case dt of
        Left (Tuple false _) → resetChildErrorBasedOnMessage msg
        _ → pure unit
      pure dt
    Just (Left err) → buildDateTime format
    Just (Right dt) → pure $ lmap (Tuple false) case msg of
      Left  (NotifyChange newDate) → case newDate of
        Just (Right date) → Right $ setDateDt date dt
        Just (Left x) → Left $ dateError x
        Nothing → Left $ emptyError
      Right (NotifyChange newTime) → case newTime of
        Just (Right time) → Right $ setTimeDt time dt
        Just (Left x) → Left $ timeError x
        Nothing → Left $ emptyError
  pure next

resetChildErrorBasedOnMessage ∷ ∀ m. MessageIn → DSL m Unit
resetChildErrorBasedOnMessage (Left (NotifyChange (Just (Left _)))) = resetDate
resetChildErrorBasedOnMessage (Right (NotifyChange (Just (Left _)))) = resetTime
resetChildErrorBasedOnMessage _ = pure unit

resetChildError ∷ ∀ m. F.Format → DSL m Unit
resetChildError format = do
  for_ (unwrap format) case _ of
    F.Time _ → resetTime
    F.Date _ → resetDate


timeError ∷ TimeError → DateTimeError
timeError x = Tuple Nothing (Just x)

dateError ∷ DateError → DateTimeError
dateError x = Tuple (Just x) Nothing

setTimeDt ∷ Time → DateTime → DateTime
setTimeDt x dt = modifyTime (const x) dt
setDateDt ∷ Date → DateTime → DateTime
setDateDt x dt = modifyDate (const x) dt

type BuildStep
  = Maybe
    (Join
      (Star (Writer (Maybe (Tuple (Additive Int) DateTimeErrorLast))))
      DateTime)

buildDateTime ∷ ∀ m
  . F.Format
  → DSL m (Either (Tuple Boolean DateTimeError) DateTime)
buildDateTime format = do
  buildSteps ← for (sort $ unwrap format) mkBuildStep
  pure $
    fromMaybe (Left (Tuple false emptyError)) $
    runStep (length buildSteps) (foldSteps buildSteps)
  where
  runStep
    ∷ Int
    → BuildStep
    → Maybe (Either (Tuple Boolean DateTimeError) DateTime)
  runStep childCount step = step <#> \(Join (Star f)) → case runWriter $ f bottom of
    Tuple res Nothing → Right res
    Tuple res (Just (Tuple (Additive errCount) err)) → Left $ Tuple
      -- if we hit errCount == 0 or errCount == childCount we shuoldn't force
      (errCount > 0  && errCount < childCount)
      (bimap unwrap unwrap err)
  mkBuildStep ∷ F.Command → DSL m BuildStep
  mkBuildStep = case _ of
    F.Time _ → do
      val ← queryTime $ getValue
      pure $ applyValue setTimeDt timeError val
    F.Date _ → do
      val ← queryDate $ getValue
      pure $ applyValue setDateDt dateError val
  applyValue ∷ ∀ val err
    . (val → DateTime → DateTime)
    → (err → DateTimeError)
    → PickerValue err val
    → BuildStep
  applyValue f err val = Just $ Join $ Star $ \dt → case val of
    Just (Right x) → pure $ f x dt
    Just (Left x) → writeErr dt (Additive 1) (biLast $ err x)
    Nothing → writeErr dt (Additive 0) mempty
  writeErr dt a b = tell (Just $ Tuple a b) *> pure dt
  biLast = bimap Last Last


evalPicker ∷ ∀ m. F.Format → QueryIn ~> DSL m
evalPicker format (ResetError next) = do
  H.put Nothing
  resetChildError format
  pure next
evalPicker format (Base (SetValue dateTime reply)) = do
  propagateChange format dateTime
  H.put dateTime
  pure $ reply unit
evalPicker _ (Base (GetValue reply)) = H.get <#> reply

propagateChange ∷ ∀ m . F.Format → State → DSL m Unit
propagateChange format dateTime = for_ (unwrap format) case _ of
  F.Time _ → setTime $ value dateTime <#> (time >>> Right)
  F.Date _ → setDate $ value dateTime <#> (date >>> Right)

emptyError ∷ DateTimeError
emptyError = Tuple Nothing Nothing

setTime ∷ ∀ m. PickerValue TimeError Time → DSL m Unit
setTime val = queryTime $ setValue val

setDate ∷ ∀ m. PickerValue DateError Date → DSL m Unit
setDate val = queryDate $ setValue val

resetTime ∷ ∀ m. DSL m Unit
resetTime = queryTime $ resetError

resetDate ∷ ∀ m. DSL m Unit
resetDate = queryDate $ resetError

queryTime ∷ ∀ m a. Time.Query a → DSL m a
queryTime q = H.query' cpTime unit q >>= mustBeMounted

queryDate ∷ ∀ m a. Date.Query a → DSL m a
queryDate q = H.query' cpDate unit q >>= mustBeMounted
