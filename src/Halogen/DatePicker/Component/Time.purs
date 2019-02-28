module Halogen.Datepicker.Component.Time where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Data.Array (sort)
import Data.DateTime (Hour, Millisecond, Minute, Second)
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, fromEnum, upFromIncluding)
import Data.Foldable (for_)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Newtype (unwrap)
import Data.Profunctor.Join (Join(..))
import Data.Profunctor.Star (Star(..))
import Data.Symbol (SProxy(..))
import Data.Time (Time)
import Data.Traversable (for)
import Effect.Exception as Ex
import Halogen as H
import Halogen.Datepicker.Component.Types (BasePickerQuery(..), PickerQuery, PickerValue, value)
import Halogen.Datepicker.Config (Config, defaultConfig)
import Halogen.Datepicker.Format.Time as F
import Halogen.Datepicker.Internal.Choice as Choice
import Halogen.Datepicker.Internal.Elements (PreChoiceConfig, PreNumConfig, renderChoice, renderNum, textElement)
import Halogen.Datepicker.Internal.Enums (Hour12, Meridiem, Millisecond1, Millisecond2)
import Halogen.Datepicker.Internal.Num as Num
import Halogen.Datepicker.Internal.Range (Range, bottomTop)
import Halogen.Datepicker.Internal.Utils (componentProps, foldSteps, handlePickerQuery, mustBeMounted, pickerProps, transitionState')
import Halogen.HTML as HH

type State = PickerValue TimeError Time

type Message = PickerValue TimeError Time

type Query = PickerQuery Unit State
type Action = Time → Maybe Time

data TimeError = InvalidTime
derive instance timeErrorEq ∷ Eq TimeError
derive instance timeErrorOrd ∷ Ord TimeError
derive instance timeErrorGeneric ∷ Generic TimeError _
instance timeErrorShow ∷ Show TimeError where show = genericShow

type Slots =
  ( num ∷ Num.Slot Int F.Command
  , choice ∷ Choice.Slot (Maybe Int) F.Command
  )

_num = SProxy ∷ SProxy "num"
_choice = SProxy ∷ SProxy "choice"

type Slot = H.Slot Query Message

type HTML m = H.ComponentHTML Action Slots m
type DSL = H.HalogenM State Action Slots Message

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
    , render: render config format
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction format
        , handleQuery = handlePickerQuery (propagateChange format)
        }
    }

render ∷ ∀ m. Config → F.Format → State → HTML m
render config format time = HH.ul
  (pickerProps config time)
  (unwrap format <#> renderCommand config)

renderCommand ∷ ∀ m. Config → F.Command → HTML m
renderCommand config cmd = HH.li (componentProps config) $ pure case cmd of
  F.Placeholder str →
    textElement config { text: str}
  F.Meridiem → renderChoice'
    { title: "Meridiem", values: upFromIncluding (bottom ∷ Maybe Meridiem) }
  F.Hours24 → renderNum'
    { title: "Hours", placeholder: "HH", range: (bottomTop ∷ Range Hour) <#> fromEnum }
  F.Hours12 → renderNum'
    { title: "Hours", placeholder: "hh", range: (bottomTop ∷ Range Hour12) <#> fromEnum }
  F.MinutesTwoDigits → renderNum'
    { title: "Minutes", placeholder: "MM", range: (bottomTop ∷ Range Minute) <#> fromEnum }
  F.Minutes → renderNum'
    { title: "Minutes", placeholder: "MM", range: (bottomTop ∷ Range Minute) <#> fromEnum }
  F.SecondsTwoDigits → renderNum'
    { title: "Seconds", placeholder: "SS", range: (bottomTop ∷ Range Second) <#> fromEnum }
  F.Seconds → renderNum'
    { title: "Seconds", placeholder: "SS", range: (bottomTop ∷ Range Second) <#> fromEnum }
  F.Milliseconds → renderNum'
    { title: "Milliseconds", placeholder: "MMM", range: (bottomTop ∷ Range Millisecond) <#> fromEnum }
  F.MillisecondsTwoDigits → renderNum'
    { title: "Milliseconds", placeholder: "MM", range: (bottomTop ∷ Range Millisecond2) <#> fromEnum }
  F.MillisecondsShort → renderNum'
    { title: "Milliseconds", placeholder: "M", range: (bottomTop ∷ Range Millisecond1) <#> fromEnum }
  where
  renderNum' ∷ PreNumConfig Int → HTML m
  renderNum' = renderNum _num F.toSetter cmd config
  renderChoice' ∷ ∀ a. BoundedEnum a ⇒ Show a ⇒ PreChoiceConfig (Maybe a) → HTML m
  renderChoice' = renderChoice _choice F.toSetter cmd config

handleAction ∷ ∀ m.  MonadError Ex.Error m ⇒ F.Format → Action → DSL m Unit
handleAction format update = do
  transitionState' InvalidTime \time → map (maybe (Left false) Right) $
    case time of
      Just (Right prevTime) → pure $ update prevTime
      _  → buildTime format

type BuildStep = Maybe (Join (Star Maybe) Time)

buildTime ∷ ∀ m. MonadError Ex.Error m ⇒ F.Format → DSL m (Maybe Time)
buildTime format = do
  buildSteps ← for (sort $ unwrap format) mkBuildStep
  pure $ runStep $ foldSteps buildSteps
  where
  runStep ∷ BuildStep → Maybe Time
  runStep step = step >>= \(Join (Star f)) → f bottom
  mkBuildStep ∷ F.Command → DSL m BuildStep
  mkBuildStep cmd = case cmd of
    F.Placeholder _ → do
      pure $ Just $ mempty
    F.Meridiem → do
      num ← queryChoice cmd $ H.request GetValue
      pure $ num <#> \n → Join $ Star $ \t → F.toSetter cmd n t
    _ → do
      num ← queryNum cmd $ H.request GetValue
      pure $ num <#> \n → Join $ Star $ \t → F.toSetter cmd n t

propagateChange ∷ ∀ m. MonadError Ex.Error m ⇒ F.Format → State → DSL m Unit
propagateChange format time = for_ (unwrap format) \cmd → case cmd of
  F.Placeholder _ → pure unit
  F.Meridiem → do
    let val = value time >>= F.toGetter F.Meridiem
    res ← queryChoice cmd $ H.request (SetValue val)
    Choice.valueMustBeInValues res
  _ → do
    let val = value time >>= F.toGetter cmd
    queryNum cmd $ H.request (SetValue val)

queryChoice ∷ ∀ m. MonadError Ex.Error m ⇒ F.Command → Choice.Query (Maybe Int) ~> DSL m
queryChoice s q = H.query _choice s q >>= mustBeMounted

queryNum ∷ ∀ m. MonadError Ex.Error m ⇒ F.Command → Num.Query Int ~> DSL m
queryNum s q = H.query _num s q >>= mustBeMounted
