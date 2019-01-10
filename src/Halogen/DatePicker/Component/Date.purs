module Halogen.Datepicker.Component.Date where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Data.Array (sort)
import Data.Date (Date, Day, Month, Year)
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, fromEnum, toEnum, upFromIncluding)
import Data.Foldable (for_)
import Data.Functor.Coproduct (Coproduct, coproduct, right, left)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Profunctor.Join (Join(..))
import Data.Profunctor.Star (Star(..))
import Data.Symbol (SProxy(..))
import Data.Traversable (for)
import Effect.Exception as Ex
import Halogen as H
import Halogen.Datepicker.Component.Types (BasePickerQuery(..), PickerMessage, PickerQuery(..), PickerValue, value)
import Halogen.Datepicker.Config (Config, defaultConfig)
import Halogen.Datepicker.Format.Date as F
import Halogen.Datepicker.Internal.Choice as Choice
import Halogen.Datepicker.Internal.Elements (textElement, PreChoiceConfig, renderChoice, renderNum)
import Halogen.Datepicker.Internal.Enums (MonthShort, Year2, Year4, setYear)
import Halogen.Datepicker.Internal.Num as Num
import Halogen.Datepicker.Internal.Range (Range, bottomTop)
import Halogen.Datepicker.Internal.Utils (componentProps, foldSteps, mapComponentHTMLQuery, mustBeMounted, pickerProps, transitionState')
import Halogen.HTML as HH

type State = PickerValue DateError Date

type Message = PickerMessage State

type Query = Coproduct QueryIn DateQuery
type QueryIn = PickerQuery Unit State
data DateQuery a = Update (Date → Maybe Date) a

data DateError = InvalidDate
derive instance dateErrorEq ∷ Eq DateError
derive instance dateErrorOrd ∷ Ord DateError
derive instance dateErrorGeneric ∷ Generic DateError _
instance dateErrorShow ∷ Show DateError where
  show = genericShow

type Slots =
  ( num ∷ (Num.Slot Int) F.Command
  , choice ∷ (Choice.Slot (Maybe Int)) F.Command
  )

_num = SProxy ∷ SProxy "num"
_choice = SProxy ∷ SProxy "choice"

type Slot = H.Slot Query Message

type HTML m = H.ComponentHTML DateQuery Slots m
type DSL = H.HalogenM State Query Slots Message


picker
  ∷ ∀ m
  . MonadError Ex.Error m
  ⇒ F.Format → H.Component HH.HTML Query Unit Message m
picker = pickerWithConfig defaultConfig

pickerWithConfig
  ∷ ∀ m
  . MonadError Ex.Error m
  ⇒ Config
  → F.Format
  → H.Component HH.HTML Query Unit Message m
pickerWithConfig config format = H.component
  { initialState: const Nothing
  , render: render config format >>> mapComponentHTMLQuery right
  , eval: coproduct (evalPicker format) (evalDate format)
  , receiver: const Nothing
  , initializer: Nothing
  , finalizer: Nothing
  }

render ∷ ∀ m. Config → F.Format → State → HTML m
render config format date = HH.ul
  (pickerProps config date)
  (unwrap format <#> renderCommand config)

renderCommand ∷ ∀ m. Config → F.Command → HTML m
renderCommand config cmd = HH.li (componentProps config) $ pure case cmd of
  F.Placeholder str →
    textElement config { text: str}
  F.YearFull → renderNum'
     { title: "Year", placeholder: "YYYY", range: (bottomTop ∷ Range Year4) <#> fromEnum }
  F.YearTwoDigits → renderNum'
     { title: "Year", placeholder: "YY", range: (bottomTop ∷ Range Year2) <#> fromEnum }
  F.YearAbsolute → renderNum'
     { title: "Year", placeholder: "Y", range: (bottomTop ∷ Range Year) <#> fromEnum }
  F.MonthFull → renderChoice'
    { title: "Month", values: upFromIncluding (bottom ∷ Maybe Month) }
  F.MonthShort → renderChoice'
    { title: "Month", values: upFromIncluding (bottom ∷ Maybe MonthShort) }
  F.MonthTwoDigits → renderNum'
     { title: "Month", placeholder: "MM", range: (bottomTop ∷ Range Month) <#> fromEnum }
  F.DayOfMonthTwoDigits → renderNum'
     { title: "Day", placeholder: "DD", range: (bottomTop ∷ Range Day) <#> fromEnum }
  F.DayOfMonth → renderNum'
     { title: "Day", placeholder: "D", range: (bottomTop ∷ Range Day) <#> fromEnum }
  where
  renderNum' = renderNum _num Update F.toSetter cmd config
  renderChoice' ∷ ∀ a. BoundedEnum a ⇒ Show a ⇒ PreChoiceConfig (Maybe a) → HTML m
  renderChoice' = renderChoice _choice Update F.toSetter cmd config

evalDate ∷ ∀ m. MonadError Ex.Error m ⇒ F.Format → DateQuery ~> DSL m
evalDate format (Update update next) = do
  transitionState' InvalidDate case _ of
    Just (Right prevDate) → pure $ maybe (Left false) Right $ update prevDate
    _ → buildDate format
  pure next

type BuildStep = Maybe (Join (Star Maybe) Date)
buildDate ∷ ∀ m. MonadError Ex.Error m ⇒ F.Format → DSL m (Either Boolean Date)
buildDate format = do
  buildSteps ← for (sort $ unwrap format) $ mkBuildStep
  pure case runStep $ foldSteps buildSteps of
    Just (Just x) → Right x
    Just Nothing → Left true
    Nothing → Left false
  where
  mkBuildStep ∷ F.Command → DSL m BuildStep
  mkBuildStep = commandCata
    { text: \cmd → pure $ Just mempty
    , enum: \cmd → do
        num ← queryNum cmd $ H.request (left <<< GetValue)
        pure $ num <#> \n → Join $ Star $ \t → F.toSetter cmd n t
    , choice: \cmd → do
        num ← queryChoice cmd $ H.request (left <<< GetValue)
        pure $ num <#> \n → Join $ Star $ \t → F.toSetter cmd n t
    }
  runStep ∷ BuildStep → Maybe (Maybe Date)
  runStep step = step <#> \(Join (Star f)) ->
    (toEnum 0) >>= (_ `setYear` bottom) >>= f

evalPicker ∷ ∀ m. MonadError Ex.Error m ⇒ F.Format → QueryIn ~> DSL m
evalPicker _ (ResetError next) = do
  H.put Nothing
  pure next
evalPicker format (Base (SetValue date reply)) = do
  propagateChange format date
  H.put date
  pure $ reply unit
evalPicker _ (Base (GetValue reply)) = H.get <#> reply

propagateChange
  ∷ ∀ m
  . MonadError Ex.Error m
  ⇒ F.Format
  → State
  → DSL m Unit
propagateChange format date = for_ (unwrap format) $ commandCata
  { text: \cmd → pure unit
  , enum: \cmd → do
    let val = value date >>= F.toGetter cmd
    queryNum cmd $ H.request $ left <<< SetValue val
  , choice: \cmd → do
    let val = value date >>= F.toGetter cmd
    res ← queryChoice cmd $ H.request $ left <<< SetValue val
    Choice.valueMustBeInValues res
  }

commandCata
  ∷ ∀ a
  . { text   ∷ F.Command → a
    , enum   ∷ F.Command → a
    , choice ∷ F.Command → a
    }
  → F.Command
  → a
commandCata p cmd = case cmd of
  F.Placeholder str     → p.text cmd
  F.YearFull            → p.enum cmd
  F.YearTwoDigits       → p.enum cmd
  F.YearAbsolute        → p.enum cmd
  F.MonthFull           → p.choice cmd
  F.MonthShort          → p.choice cmd
  F.MonthTwoDigits      → p.enum cmd
  F.DayOfMonthTwoDigits → p.enum cmd
  F.DayOfMonth          → p.enum cmd

queryChoice
  ∷ ∀ m
  . MonadError Ex.Error m
  ⇒ F.Command
  → Choice.Query (Maybe Int)
  ~> DSL m
queryChoice s q = H.query _choice s q >>= mustBeMounted

queryNum
  ∷ ∀ m
  . MonadError Ex.Error m
  ⇒ F.Command
  → Num.Query Int
  ~> DSL m
queryNum s q = H.query _num s q >>= mustBeMounted
