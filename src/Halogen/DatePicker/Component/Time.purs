module Halogen.Datepicker.Component.Time where

import Prelude

import Data.Array (sort)
import Data.Bifunctor (bimap)
import Data.DateTime (Hour, Millisecond, Minute, Second)
import Data.Either (Either(..))
import Data.Either.Nested (Either2)
import Data.Enum (class BoundedEnum, fromEnum, toEnum, upFromIncluding)
import Data.Foldable (for_)
import Data.Functor.Coproduct (Coproduct, coproduct, right, left)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Monoid (mempty)
import Data.Newtype (unwrap)
import Data.NonEmpty (NonEmpty)
import Data.Profunctor.Join (Join(..))
import Data.Profunctor.Star (Star(..))
import Data.Time (Time)
import Data.Traversable (for)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.Datepicker.Component.Types (BasePickerQuery(..), PickerMessage(..), PickerQuery(..), PickerValue, value)
import Halogen.Datepicker.Format.Time as F
import Halogen.Datepicker.Internal.Choice as Choice
import Halogen.Datepicker.Internal.Elements (textElement)
import Halogen.Datepicker.Internal.Enums (Hour12, Meridiem, Millisecond1, Millisecond2)
import Halogen.Datepicker.Internal.Num as Num
import Halogen.Datepicker.Internal.Range (Range, bottomTop)
import Halogen.Datepicker.Internal.Utils (componentProps, foldSteps, mustBeMounted, pickerProps, transitionState')
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

data TimeQuery a = Update (Time → Maybe Time) a
type State = PickerValue TimeError Time
type QueryIn = PickerQuery Unit State
type Query = Coproduct QueryIn TimeQuery
type Message = PickerMessage State

data TimeError = InvalidTime
derive instance timeErrorEq ∷ Eq TimeError
derive instance timeErrorOrd ∷ Ord TimeError
derive instance timeErrorGeneric ∷ Generic TimeError _
instance timeErrorShow ∷ Show TimeError where
  show = genericShow


type NumQuery = Num.Query Int
type ChoiceQuery = Choice.Query (Maybe Int)
type ChildQuery = Coproduct2 NumQuery ChoiceQuery

type ChoiceSlot = F.Command
type NumSlot = F.Command
type Slot = Either2 ChoiceSlot NumSlot

cpNum ∷ CP.ChildPath NumQuery ChildQuery F.Command Slot
cpNum = CP.cp1

cpChoice ∷ CP.ChildPath ChoiceQuery ChildQuery F.Command Slot
cpChoice = CP.cp2

type HTML m = H.ParentHTML TimeQuery ChildQuery Slot m
type DSL m = H.ParentDSL State Query ChildQuery Slot Message m

picker ∷ ∀ m. F.Format → H.Component HH.HTML Query Unit Message m
picker format = H.parentComponent
  { initialState: const Nothing
  , render: render format >>> bimap (map right) right
  , eval: coproduct (evalPicker format) (evalTime format)
  , receiver: const Nothing
  }

render ∷ ∀ m. F.Format → State → HTML m
render format time = HH.ul (pickerProps time) (unwrap format <#> renderCommand)

renderCommand ∷ ∀ m. F.Command → HTML m
renderCommand cmd = HH.li componentProps $ pure case cmd of
  F.Placeholder str →
    textElement { text: str}
  F.Meridiem →
    renderCommandChoice cmd { title: "Meridiem", values: upFromIncluding (bottom ∷ Maybe Meridiem) }
  F.Hours24 →
    renderCommandEnum cmd { title: "Hours", range: (bottomTop ∷ Range Hour) <#> fromEnum }
  F.Hours12 →
    renderCommandEnum cmd { title: "Hours", range: (bottomTop ∷ Range Hour12) <#> fromEnum }
  F.MinutesTwoDigits →
    renderCommandEnum cmd { title: "Minutes", range: (bottomTop ∷ Range Minute) <#> fromEnum }
  F.Minutes →
    renderCommandEnum cmd { title: "Minutes", range: (bottomTop ∷ Range Minute) <#> fromEnum }
  F.SecondsTwoDigits →
    renderCommandEnum cmd { title: "Seconds", range: (bottomTop ∷ Range Second) <#> fromEnum }
  F.Seconds →
    renderCommandEnum cmd { title: "Seconds", range: (bottomTop ∷ Range Second) <#> fromEnum }
  F.Milliseconds →
    renderCommandEnum cmd { title: "Milliseconds", range: (bottomTop ∷ Range Millisecond) <#> fromEnum }
  F.MillisecondsTwoDigits →
    renderCommandEnum cmd { title: "Milliseconds", range: (bottomTop ∷ Range Millisecond2) <#> fromEnum }
  F.MillisecondsShort →
    renderCommandEnum cmd { title: "Milliseconds", range: (bottomTop ∷ Range Millisecond1) <#> fromEnum }

renderCommandEnum ∷ ∀ m
  . F.Command
  → { title ∷ String , range  ∷ Range Int }
  → HTML m
renderCommandEnum cmd conf' = let conf = conf'{range = conf'.range} in
  HH.slot' cpNum cmd
    (Num.picker Num.intHasNumberInputVal conf) unit
    (HE.input $ \(NotifyChange n) → Update $ \t → n >>= (_ `F.toSetter cmd` t))

renderCommandChoice ∷ ∀ m a
  . BoundedEnum a
  ⇒ Show a
  ⇒ F.Command
  → { title ∷ String , values ∷ NonEmpty Array (Maybe a) }
  → HTML m
renderCommandChoice cmd conf = HH.slot' cpChoice cmd
    (Choice.picker
      (Choice.maybeIntHasChoiceInputVal \n → (toEnum n ∷ Maybe a) # maybe "" show)
      (conf{values = conf.values <#> map fromEnum})
    )
    unit
    (HE.input $ \(NotifyChange n) → Update $ \t → n >>= (_ `F.toSetter cmd` t))

evalTime ∷ ∀ m . F.Format → TimeQuery ~> DSL m
evalTime format (Update update next) = do
  transitionState' InvalidTime \time → map (maybe (Left false) Right) $
    case time of
      Just (Right prevTime) → pure $ update prevTime
      _  → buildTime format
  pure next

type BuildStep = Maybe (Join (Star Maybe) Time)
buildTime ∷ ∀ m. F.Format → DSL m (Maybe Time)
buildTime format = do
  buildSteps ← for (sort $ unwrap format) mkBuildStep
  pure $ runStep $ foldSteps buildSteps
  where
  runStep ∷ BuildStep -> Maybe Time
  runStep step = step >>= \(Join (Star f)) → f bottom
  mkBuildStep ∷ F.Command → DSL m BuildStep
  mkBuildStep cmd = case cmd of
    F.Placeholder _ → do
      pure $ Just $ mempty
    F.Meridiem → do
      num ← queryChoice cmd $ H.request (left <<< GetValue)
      pure $ num <#> \n → Join $ Star $ \t → F.toSetter cmd n t
    _ → do
      num ← queryNum cmd $ H.request (left <<< GetValue)
      pure $ num <#> \n → Join $ Star $ \t → F.toSetter cmd n t


evalPicker ∷ ∀ m. F.Format → QueryIn ~> DSL m
evalPicker _ (ResetError next) = do
  H.put Nothing
  pure next
evalPicker format (Base (SetValue time reply)) = do
  propagateChange format time
  H.put time
  pure $ reply unit
evalPicker _ (Base (GetValue reply)) = H.get <#> reply

propagateChange ∷ ∀ m . F.Format → State → DSL m Unit
propagateChange format time = for_ (unwrap format) \cmd → case cmd of
  F.Placeholder _ → pure unit
  F.Meridiem → do
    let val = value time >>= F.toGetter F.Meridiem
    res ← queryChoice cmd $ H.request $ left <<< SetValue val
    Choice.valueMustBeInValues res
  _ → do
    let val = value time >>= F.toGetter cmd
    queryNum cmd $ H.request $ left <<< SetValue val

queryChoice ∷ ∀ m. ChoiceSlot → ChoiceQuery ~> DSL m
queryChoice s q = H.query' cpChoice s q >>= mustBeMounted

queryNum ∷ ∀ m. NumSlot → NumQuery ~> DSL m
queryNum s q = H.query' cpNum s q >>= mustBeMounted
