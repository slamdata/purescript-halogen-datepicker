module Halogen.Datepicker.Component.DateTime where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Array (sort)
import Data.Bifunctor (bimap, lmap)
import Data.Date (Date)
import Data.DateTime (DateTime, date, modifyDate, modifyTime, time)
import Data.Either (Either(..))
import Data.Foldable (length)
import Data.Functor.Coproduct (Coproduct, right)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe.Last (Last(..))
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.Profunctor.Join (Join(..))
import Data.Profunctor.Star (Star(..))
import Data.Symbol (SProxy(..))
import Data.Time (Time)
import Data.Traversable (for, for_)
import Data.Tuple (Tuple(..))
import Effect.Exception as Ex
import Halogen as H
import Halogen.Datepicker.Component.Date (DateError)
import Halogen.Datepicker.Component.Date as Date
import Halogen.Datepicker.Component.Time (TimeError)
import Halogen.Datepicker.Component.Time as Time
import Halogen.Datepicker.Component.Types (BasePickerQuery(..), PickerMessage(..), PickerQuery(..), PickerValue, value, getValue, setValue, resetError)
import Halogen.Datepicker.Config (Config, defaultConfig)
import Halogen.Datepicker.Format.DateTime as F
import Halogen.Datepicker.Internal.Utils (componentProps, foldSteps, mapComponentHTMLQuery, mkEval, mustBeMounted, pickerProps, transitionState)
import Halogen.HTML as HH

type State = PickerValue DateTimeError DateTime
type DateTimeError = DateTimeErrorF Maybe
type DateTimeErrorF f = Tuple (f DateError) (f TimeError)
type DateTimeErrorLast = DateTimeErrorF Last

type Message = PickerMessage State

type Query = Coproduct QueryIn DateTimeQuery
type QueryIn = PickerQuery Unit State
data DateTimeQuery a = Update MessageIn a
type MessageIn = Either Date.Message Time.Message

type Slots =
  ( date ∷ Date.Slot Unit
  , time ∷ Time.Slot Unit
  )

_date = SProxy ∷ SProxy "date"
_time = SProxy ∷ SProxy "time"

type Slot = H.Slot Query Message

type HTML m = H.ComponentHTML (DateTimeQuery Unit) Slots m
type DSL = H.HalogenM State (Query Unit) Slots Message

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
    , render: render config format >>> mapComponentHTMLQuery right
    , eval: mkEval (evalPicker format) (evalDateTime format)
    }

render ∷ ∀ m. MonadError Ex.Error m ⇒ Config → F.Format → State → HTML m
render config format dateTime = HH.div
  (pickerProps config dateTime)
  (unwrap format <#> renderCommand config)

renderCommand ∷ ∀ m. MonadError Ex.Error m ⇒ Config → F.Command → HTML m
renderCommand config cmd = HH.div (componentProps config) $ pure case cmd of
  F.Time fmt →
    HH.slot
      _time
      unit
      (Time.pickerWithConfig config fmt)
      unit
      (\val → Just (Update (Right val) unit))
  F.Date fmt →
    HH.slot
      _date
      unit
      (Date.pickerWithConfig config fmt)
      unit
      (\val → Just (Update (Left val) unit))

evalDateTime ∷ ∀ m. MonadError Ex.Error m ⇒ F.Format → DateTimeQuery ~> DSL m
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

resetChildErrorBasedOnMessage ∷ ∀ m. MonadError Ex.Error m ⇒ MessageIn → DSL m Unit
resetChildErrorBasedOnMessage (Left (NotifyChange (Just (Left _)))) = resetDate
resetChildErrorBasedOnMessage (Right (NotifyChange (Just (Left _)))) = resetTime
resetChildErrorBasedOnMessage _ = pure unit

resetChildError ∷ ∀ m. MonadError Ex.Error m ⇒ F.Format → DSL m Unit
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

buildDateTime
  ∷ ∀ m
  . MonadError Ex.Error m
  ⇒ F.Format
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

evalPicker ∷ ∀ m. MonadError Ex.Error m ⇒ F.Format → QueryIn ~> DSL m
evalPicker format (ResetError next) = do
  H.put Nothing
  resetChildError format
  pure next
evalPicker format (Base (SetValue dateTime reply)) = do
  propagateChange format dateTime
  H.put dateTime
  pure $ reply unit
evalPicker _ (Base (GetValue reply)) = H.get <#> reply

propagateChange ∷ ∀ m. MonadError Ex.Error m ⇒ F.Format → State → DSL m Unit
propagateChange format dateTime = for_ (unwrap format) case _ of
  F.Time _ → setTime $ value dateTime <#> (time >>> Right)
  F.Date _ → setDate $ value dateTime <#> (date >>> Right)

emptyError ∷ DateTimeError
emptyError = Tuple Nothing Nothing

setTime ∷ ∀ m. MonadError Ex.Error m ⇒ PickerValue TimeError Time → DSL m Unit
setTime val = queryTime $ setValue val

setDate ∷ ∀ m. MonadError Ex.Error m ⇒ PickerValue DateError Date → DSL m Unit
setDate val = queryDate $ setValue val

resetTime ∷ ∀ m. MonadError Ex.Error m ⇒ DSL m Unit
resetTime = queryTime resetError

resetDate ∷ ∀ m. MonadError Ex.Error m ⇒ DSL m Unit
resetDate = queryDate resetError

queryTime ∷ ∀ m. MonadError Ex.Error m ⇒ Time.Query ~> DSL m
queryTime q = H.query _time unit q >>= mustBeMounted

queryDate ∷ ∀ m. MonadError Ex.Error m ⇒ Date.Query ~> DSL m
queryDate q = H.query _date unit q >>= mustBeMounted
