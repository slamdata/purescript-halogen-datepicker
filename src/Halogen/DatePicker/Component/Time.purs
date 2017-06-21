module Halogen.Datepicker.Component.Time where

import Prelude

import Data.Array (sort)
import Data.Bifunctor (bimap)
import Data.DateTime (Hour, Millisecond, Minute, Second)
import Data.Either (Either(..))
import Data.Either.Nested (Either2)
import Data.Enum (class BoundedEnum, fromEnum, toEnum, upFromIncluding)
import Data.Foldable (fold)
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
import Data.Traversable (for, sequence)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.Datepicker.Component.Types (BasePickerQuery(..), PickerMessage(..), PickerQuery(..), PickerValue, value)
import Halogen.Datepicker.Format.Time as F
import Halogen.Datepicker.Internal.Choice as Choice
import Halogen.Datepicker.Internal.Elements (textElement)
import Halogen.Datepicker.Internal.Enums (Hour12, Meridiem, Millisecond1, Millisecond2)
import Halogen.Datepicker.Internal.Num as Num
import Halogen.Datepicker.Internal.Range (Range, bottomTop)
import Halogen.Datepicker.Internal.Utils (componentProps, steper', pickerProps, mustBeMounted)
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

type Slot = Either2 F.Command F.Command

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


renderCommandEnum ∷ ∀ m. F.Command → { title ∷ String , range  ∷ Range Int } → HTML m
renderCommandEnum cmd conf' = let conf = conf'{range = conf'.range} in
  HH.slot' cpNum cmd
    (Num.picker Num.intHasNumberInputVal conf) unit
    (HE.input $ \(NotifyChange n) → Update $ \t → n >>= (_ `F.toSetter cmd` t))

renderCommandChoice ∷ ∀ m a. BoundedEnum a => Show a => F.Command → { title ∷ String , values ∷ NonEmpty Array (Maybe a) } → HTML m
renderCommandChoice cmd conf = HH.slot' cpChoice cmd
    (Choice.picker
      (Choice.maybeIntHasChoiceInputVal \n → (toEnum n ∷ Maybe a) # maybe "" show)
      (conf{values = conf.values <#> map fromEnum})
    )
    unit
    (HE.input $ \(NotifyChange n) → Update $ \t → n >>= (_ `F.toSetter cmd` t))

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

evalTime ∷ ∀ m . F.Format → TimeQuery ~> DSL m
evalTime format (Update update next) = do
  time <- H.get
  nextTime <- map (steper' time InvalidTime <<< maybe (Left false) Right) $
    case time of
      Just (Right prevTime) → pure $ update prevTime
      _  → buildTime format
  H.put nextTime
  unless (nextTime == time) $ H.raise (NotifyChange nextTime)
  pure next


buildTime ∷ ∀ m. F.Format → DSL m (Maybe Time)
buildTime format = do
  mbKleisliEndo <- for (sort $ unwrap format) mkKleisli
  pure $ map fold (sequence mbKleisliEndo) >>= \(Join (Star f)) → f bottom
  where
  mkKleisli (F.Placeholder _) = pure $ Just $ mempty
  mkKleisli cmd@F.Meridiem = do
    num <- H.query' cpChoice cmd $ H.request (left <<< GetValue)
    pure $ join num <#> \n → Join $ Star $ \t → F.toSetter cmd n t
  mkKleisli cmd = do
    num <- H.query' cpNum cmd $ H.request (left <<< GetValue)
    pure $ join num <#> \n → Join $ Star $ \t → F.toSetter cmd n t


evalPicker ∷ ∀ m. F.Format → QueryIn ~> DSL m
evalPicker _ (ResetError next) = do
  H.put Nothing
  pure next
evalPicker format (Base (SetValue time next)) = do
  propagateChange format time
  H.put time
  pure $ next unit
evalPicker _ (Base (GetValue next)) = H.get <#> next

propagateChange ∷ ∀ m . F.Format → State → DSL m Unit
propagateChange format time = do
  map (mustBeMounted <<< fold) $ for (unwrap format) change
  where
  change ∷ F.Command → DSL m (Maybe Unit)
  change (F.Placeholder _ ) = pure (Just unit)
  change cmd@F.Meridiem = do
    res <- H.query' cpChoice cmd $ H.request $ left <<< SetValue m
    pure $ res >>= case _ of
      Just Choice.ValueIsNotInValues → Nothing
      Nothing → Just unit
    where
    m ∷ Maybe Int
    m = value time >>= F.toGetter F.Meridiem
  change cmd = do
    let n = value time >>= F.toGetter cmd
    H.query' cpNum cmd $ H.request $ left <<< SetValue n
