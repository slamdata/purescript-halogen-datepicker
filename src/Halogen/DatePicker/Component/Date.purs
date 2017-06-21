module Halogen.Datepicker.Component.Date where

import Prelude

import Data.Array (sort)
import Data.Bifunctor (bimap)
import Data.Date (Date, Day, Month, Year)
import Data.Either (Either(..))
import Data.Either.Nested (Either2)
import Data.Enum (class BoundedEnum, fromEnum, toEnum, upFromIncluding)
import Data.Functor.Coproduct (Coproduct, coproduct, right, left)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.Newtype (unwrap)
import Data.NonEmpty (NonEmpty)
import Data.Profunctor.Join (Join(..))
import Data.Profunctor.Star (Star(..))
import Data.Traversable (fold, for, sequence)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.Datepicker.Component.Types (BasePickerQuery(..), PickerMessage(..), PickerQuery(..), PickerValue, value)
import Halogen.Datepicker.Format.Date as F
import Halogen.Datepicker.Internal.Choice as Choice
import Halogen.Datepicker.Internal.Elements (textElement)
import Halogen.Datepicker.Internal.Enums (MonthShort, Year2, Year4, setYear)
import Halogen.Datepicker.Internal.Num as Num
import Halogen.Datepicker.Internal.Range (Range, bottomTop)
import Halogen.Datepicker.Internal.Utils (componentProps, steper', pickerProps, mustBeMounted)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

data DateQuery a = Update (Date → Maybe Date) a
type State = PickerValue DateError Date
type QueryIn = PickerQuery Unit State
type Query = Coproduct QueryIn DateQuery
type Message = PickerMessage State

data DateError = InvalidDate
derive instance dateErrorEq ∷ Eq DateError
derive instance dateErrorOrd ∷ Ord DateError
derive instance dateErrorGeneric ∷ Generic DateError _
instance dateErrorShow ∷ Show DateError where
  show = genericShow

type NumQuery = Num.Query Int
type ChoiceQuery = Choice.Query (Maybe Int)
type ChildQuery = Coproduct2 NumQuery ChoiceQuery

type Slot = Either2 F.Command F.Command

cpNum ∷ CP.ChildPath NumQuery ChildQuery F.Command Slot
cpNum = CP.cp1

cpChoice ∷ CP.ChildPath ChoiceQuery ChildQuery F.Command Slot
cpChoice = CP.cp2

type HTML m = H.ParentHTML DateQuery ChildQuery Slot m
type DSL m = H.ParentDSL State Query ChildQuery Slot Message m


picker ∷ ∀ m. F.Format → H.Component HH.HTML Query Unit Message m
picker format = H.parentComponent
  { initialState: const Nothing
  , render: render format >>> bimap (map right) right
  , eval: coproduct (evalPicker format) (evalDate format)
  , receiver: const Nothing
  }

render ∷ ∀ m. F.Format → State → HTML m
render format date = HH.ul (pickerProps date) (unwrap format <#> renderCommand)


renderCommandEnum ∷ ∀ m. F.Command → { title ∷ String , range  ∷ Range Int } → HTML m
renderCommandEnum cmd conf' = let conf = conf'{range = conf'.range} in
  HH.slot' cpNum cmd
    (Num.picker Num.intHasNumberInputVal conf) unit
    (HE.input $ \(NotifyChange n) → Update $ \t → n >>= (_ `F.toSetter cmd` t))

renderCommandChoice ∷ ∀ m a. BoundedEnum a => Show a => F.Command → { title ∷ String , values ∷ NonEmpty Array (Maybe a) } → HTML m
renderCommandChoice cmd conf = HH.slot' cpChoice cmd
    ( Choice.picker
      (Choice.maybeIntHasChoiceInputVal \n → (toEnum n ∷ Maybe a) # maybe "" show)
      (conf{values = conf.values <#> map fromEnum})
    )
    unit
    (HE.input $ \(NotifyChange n) → Update $ \t → n >>= (_ `F.toSetter cmd` t))


renderCommand ∷ ∀ m. F.Command → HTML m
renderCommand cmd = HH.li componentProps $ pure case cmd of
  F.Placeholder str →
    textElement { text: str}
  F.YearFull →
    renderCommandEnum cmd { title: "Year", range: (bottomTop ∷ Range Year4) <#> fromEnum }
  F.YearTwoDigits →
    renderCommandEnum cmd { title: "Year", range: (bottomTop ∷ Range Year2) <#> fromEnum }
  F.YearAbsolute →
    renderCommandEnum cmd { title: "Year", range: (bottomTop ∷ Range Year) <#> fromEnum }
  F.MonthFull →
    renderCommandChoice cmd { title: "Month", values: upFromIncluding (bottom ∷ Maybe Month) }
  F.MonthShort →
    renderCommandChoice cmd { title: "Month", values: upFromIncluding (bottom ∷ Maybe MonthShort) }
  F.MonthTwoDigits →
    renderCommandEnum cmd { title: "Month", range: (bottomTop ∷ Range Month) <#> fromEnum }
  F.DayOfMonthTwoDigits →
    renderCommandEnum cmd { title: "Day", range: (bottomTop ∷ Range Day) <#> fromEnum }
  F.DayOfMonth →
    renderCommandEnum cmd { title: "Day", range: (bottomTop ∷ Range Day) <#> fromEnum }

evalDate ∷ ∀ m . F.Format → DateQuery ~> DSL m
evalDate format (Update update next) = do
  date <- H.get
  nextDate <- map (steper' date InvalidDate) case date of
    Just (Right prevDate) → pure $ maybe (Left false) Right $ update prevDate
    _ → buildDate format
  H.put nextDate
  unless (nextDate == date) $ H.raise (NotifyChange nextDate)
  pure next


buildDate ∷ ∀ m. F.Format → DSL m (Either Boolean Date)
buildDate format = do
  mbKleisliEndo <- for (sort $ unwrap format) $ commandCata
    { text: \cmd → pure $ Just $ mempty
    , enum: \cmd → do
      num <- H.query' cpNum cmd $ H.request (left <<< GetValue)
      pure $ join num <#> \n → Join $ Star $ \t → F.toSetter cmd n t
    , choice: \cmd → do
      num <- H.query' cpChoice cmd $ H.request (left <<< GetValue)
      pure $ join num <#> \n → Join $ Star $ \t → F.toSetter cmd n t
    }
  pure case map fold (sequence mbKleisliEndo) of
    Just (Join (Star f)) → maybe (Left true) Right $  (toEnum 0) >>= (_ `setYear` bottom) >>= f
    Nothing → Left false


evalPicker ∷ ∀ m. F.Format -> QueryIn ~> DSL m
evalPicker _ (ResetError next) = do
  H.put Nothing
  pure next
evalPicker format (Base (SetValue date next)) = do
  propagateChange format date
  H.put date
  pure $ next unit
evalPicker _ (Base (GetValue next)) = H.get <#> next

propagateChange ∷ ∀ m . F.Format → State → DSL m Unit
propagateChange format date = do
  map (mustBeMounted <<< fold) $ for (unwrap format) $ commandCata
    { text: \cmd → pure (Just unit)
    , enum: \cmd → do
      let val = value date >>= F.toGetter cmd
      H.query' cpNum cmd $ H.request $ left <<< SetValue val
    , choice: \cmd → do
      let val = value date >>= F.toGetter cmd
      res <- H.query' cpChoice cmd $ H.request $ left <<< SetValue val
      pure $ res >>= case _ of
        Just Choice.ValueIsNotInValues → Nothing
        Nothing → Just unit
    }

commandCata ∷ ∀ a.
  { text   ∷ F.Command → a
  , enum   ∷ F.Command → a
  , choice ∷ F.Command → a
  } → F.Command → a
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
