module Halogen.Datapicker.Component.Date where

import Prelude
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.Datapicker.Component.Date.Format as F
import Halogen.Datapicker.Component.Internal.Choice as Choice
import Halogen.Datapicker.Component.Internal.Num as Num
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Control.MonadPlus (guard)
import Data.Bifunctor (bimap)
import Data.Date (Date, Day, Month, Year)
import Data.Either.Nested (Either2)
import Data.Enum (class BoundedEnum, fromEnum, toEnum, upFromIncluding)
import Data.Foldable (foldMap)
import Data.Functor.Coproduct (Coproduct, coproduct, right, left)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (sort)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.Newtype (unwrap)
import Data.NonEmpty (NonEmpty)
import Data.Profunctor.Join (Join(..))
import Data.Profunctor.Star (Star(..))
import Data.Traversable (fold, for, sequence)
import Halogen.Datapicker.Component.Internal.Elements (textElement)
import Halogen.Datapicker.Component.Internal.Enums (MonthShort, Year2, Year4)
import Halogen.Datapicker.Component.Internal.Range (Range, bottomTop)
import Halogen.Datapicker.Component.Types (PickerMessage(..), PickerQuery(..), PickerValue, isInvalid, mustBeMounted, stepPickerValue', value)

data DateQuery a = UpdateCommand (Date -> Maybe Date) a
type Input = PickerValue DateError Date
type QueryIn = PickerQuery Unit Input
type Query = Coproduct QueryIn DateQuery
type Message = PickerMessage Input

data DateError = InvalidDate
derive instance dateErrorEq :: Eq DateError
derive instance dateErrorOrd :: Ord DateError
derive instance dateErrorGeneric :: Generic DateError _
instance dateErrorShow :: Show DateError where
  show = genericShow

type State =
  { format :: F.Format
  , date :: Input
  }

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


picker ∷ ∀ m. F.Format -> H.Component HH.HTML Query Unit Message m
picker format = H.parentComponent
  { initialState: const {format, date: Nothing}
  , render: render >>> bimap (map right) right
  , eval: coproduct evalPicker evalDate
  , receiver: const Nothing
  }

render ∷ ∀ m. State -> HTML m
render s = HH.ul
  [ HP.classes $
    [HH.ClassName "Picker"]
    <> (guard (isInvalid s.date) $> HH.ClassName "Picker--invalid")
  ]
  (foldMap (pure <<< f) (unwrap s.format))
  where
  f cmd = HH.li [HP.classes [HH.ClassName "Picker-component"]] [renderCommand cmd]


renderCommandText :: ∀ m. F.Command -> { text  :: String } -> HTML m
renderCommandText cmd conf = textElement conf

renderCommandEnum :: ∀ m. F.Command -> { title :: String , range  :: Range Int } -> HTML m
renderCommandEnum cmd conf' = let conf = conf'{range = conf'.range} in
  HH.slot' cpNum cmd
    (Num.picker Num.intHasNumberInputVal conf) unit
    (HE.input $ \(NotifyChange n) -> UpdateCommand $ \t -> n >>= (_ `F.toSetter cmd` t))

renderCommandChoice :: ∀ m a. BoundedEnum a => Show a => F.Command -> { title :: String , values :: NonEmpty Array (Maybe a) } -> HTML m
renderCommandChoice cmd conf = HH.slot' cpChoice cmd
    ( Choice.picker
      (Choice.maybeIntHasChoiceInputVal \n -> (toEnum n :: Maybe a) # maybe "" show)
      (conf{values = conf.values <#> map fromEnum})
    )
    unit
    (HE.input $ \(NotifyChange n) -> UpdateCommand $ \t -> n >>= (_ `F.toSetter cmd` t))


renderCommand :: ∀ m. F.Command -> HTML m
renderCommand cmd = case cmd of
  F.Placeholder str     -> renderCommandText cmd { text: str}
  F.YearFull            -> renderCommandEnum cmd { title: "Year", range: (bottomTop :: Range Year4) <#> fromEnum }
  F.YearTwoDigits       -> renderCommandEnum cmd { title: "Year", range: (bottomTop :: Range Year2) <#> fromEnum }
  F.YearAbsolute        -> renderCommandEnum cmd { title: "Year", range: (bottomTop :: Range Year) <#> fromEnum }
  F.MonthFull           -> renderCommandChoice cmd { title: "Month", values: upFromIncluding (bottom :: Maybe Month) }
  F.MonthShort          -> renderCommandChoice cmd { title: "Month", values: upFromIncluding (bottom :: Maybe MonthShort) }
  F.MonthTwoDigits      -> renderCommandEnum cmd { title: "Month", range: (bottomTop :: Range Month) <#> fromEnum }
  F.DayOfMonthTwoDigits -> renderCommandEnum cmd { title: "Day", range: (bottomTop :: Range Day) <#> fromEnum }
  F.DayOfMonth          -> renderCommandEnum cmd { title: "Day", range: (bottomTop :: Range Day) <#> fromEnum }

evalDate ∷ ∀ m . DateQuery ~> DSL m
evalDate (UpdateCommand update next) = do
  s <- H.get
  (nextDate :: Input) <- stepPickerValue'
    InvalidDate
    (maybe buildDate $ update >>> pure)
    s.date
  H.modify _{ date = nextDate }
  when (nextDate /= s.date) $ H.raise (NotifyChange nextDate)
  pure next


buildDate :: ∀ m. DSL m (Maybe Date)
buildDate = do
  {format} <- H.get
  mbKleisliEndo <- for (sort $ unwrap format) $ commandCata
    { text: \cmd -> pure $ Just $ mempty
    , enum: \cmd -> do
      num <- H.query' cpNum cmd $ H.request (left <<< GetValue)
      pure $ join num <#> \n -> Join $ Star $ \t -> F.toSetter cmd n t
    , choice: \cmd -> do
      num <- H.query' cpChoice cmd $ H.request (left <<< GetValue)
      pure $ num <#> \n -> Join $ Star $ \t -> n >>= (_ `F.toSetter cmd` t)
    }
  pure $ map fold (sequence mbKleisliEndo) >>= \(Join (Star f)) -> f bottom


evalPicker ∷ ∀ m . QueryIn ~> DSL m
evalPicker (SetValue date next) = do
  {format} <- H.get
  propagateChange format date
  H.modify _{ date = date }
  pure $ next unit
evalPicker (GetValue next) = H.gets _.date <#> next

propagateChange :: ∀ m . F.Format -> Input -> DSL m Unit
propagateChange format date = do
  map (mustBeMounted <<< fold) $ for (unwrap format) $ commandCata
    { text: \cmd -> pure (Just unit)
    , enum: \cmd -> do
      let val = value date >>= F.toGetter cmd
      H.query' cpNum cmd $ H.request $ left <<< SetValue val
    , choice: \cmd -> do
      let val = value date >>= F.toGetter cmd
      res <- H.query' cpChoice cmd $ H.request $ left <<< SetValue val
      pure $ res >>= case _ of
        Just Choice.ValueIsNotInValues -> Nothing
        Nothing -> Just unit
    }

commandCata :: ∀ a.
  { text   :: F.Command -> a
  , enum   :: F.Command -> a
  , choice :: F.Command -> a
  } -> F.Command -> a
commandCata p cmd = case cmd of
  F.Placeholder str     -> p.text cmd
  F.YearFull            -> p.enum cmd
  F.YearTwoDigits       -> p.enum cmd
  F.YearAbsolute        -> p.enum cmd
  F.MonthFull           -> p.choice cmd
  F.MonthShort          -> p.choice cmd
  F.MonthTwoDigits      -> p.enum cmd
  F.DayOfMonthTwoDigits -> p.enum cmd
  F.DayOfMonth          -> p.enum cmd
