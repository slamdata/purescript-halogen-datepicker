module Halogen.Datapicker.Component.Time where

import Prelude

import Data.Bifunctor (bimap)
import Data.DateTime (Hour, Millisecond, Minute, Second)
import Data.Either (Either(..))
import Data.Either.Nested (Either2)
import Data.Enum (fromEnum, toEnum, upFromIncluding)
import Data.Foldable (fold, foldMap)
import Data.Functor.Coproduct (Coproduct, coproduct, right, left)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (sort)
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Monoid (mempty)
import Data.Newtype (unwrap)
import Data.Profunctor.Join (Join(..))
import Data.Profunctor.Star (Star(..))
import Data.Time (Time)
import Data.Traversable (for, sequence)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.Datapicker.Component.Internal.Choice as Choice
import Halogen.Datapicker.Component.Internal.Elements (textElement)
import Halogen.Datapicker.Component.Internal.Enums (Hour12, Meridiem, Millisecond1, Millisecond2)
import Halogen.Datapicker.Component.Internal.Num as Num
import Halogen.Datapicker.Component.Internal.Range (Range, bottomTop)
import Halogen.Datapicker.Component.Time.Format as F
import Halogen.Datapicker.Component.Types (PickerMessage(..), PickerQuery(..), BasePickerQuery(..), PickerValue, mustBeMounted, pickerClasses, steper', toAlt, value)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data TimeQuery a = Update (Time -> Maybe Time) a
type Input = PickerValue TimeError Time
type QueryIn = PickerQuery Unit Input
type Query = Coproduct QueryIn TimeQuery
type Message = PickerMessage Input

data TimeError = InvalidTime
derive instance timeErrorEq :: Eq TimeError
derive instance timeErrorOrd :: Ord TimeError
derive instance timeErrorGeneric :: Generic TimeError _
instance timeErrorShow :: Show TimeError where
  show = genericShow


type State =
  { format :: F.Format
  , time :: Input
  }

type NumQuery = Num.Query Int
-- TODO maybe change Meridiem to Int as in NumQuery
type ChoiceQuery = Choice.Query Meridiem
type ChildQuery = Coproduct2 NumQuery ChoiceQuery

type Slot = Either2 F.Command Unit

cpNum ∷ CP.ChildPath NumQuery ChildQuery F.Command Slot
cpNum = CP.cp1

cpChoice ∷ CP.ChildPath ChoiceQuery ChildQuery Unit Slot
cpChoice = CP.cp2

type HTML m = H.ParentHTML TimeQuery ChildQuery Slot m
type DSL m = H.ParentDSL State Query ChildQuery Slot Message m

picker ∷ ∀ m. F.Format -> H.Component HH.HTML Query Unit Message m
picker format = H.parentComponent
  { initialState: const $ {format, time: Nothing}
  , render: render >>> bimap (map right) right
  , eval: coproduct evalPicker evalTime
  , receiver: const Nothing
  }

render ∷ ∀ m. State -> HTML m
render s = HH.ul [ HP.classes $ pickerClasses s.time ]
    (foldMap (pure <<< f) (unwrap s.format))
  where
  f cmd = HH.li [HP.classes [HH.ClassName "Picker-component"]] $ renderCommand cmd

onMb :: ∀ a b. (a -> b -> Maybe b) -> Maybe a -> b -> Maybe b
onMb f a b = a >>= \a' -> f a' b

renderCommand :: ∀ m. F.Command -> Array (HTML m)
renderCommand (F.Placeholder str) = pure $ textElement { text: str}
renderCommand cmd@F.Meridiem = pure $ HH.slot'
  cpChoice
  unit
  (Choice.picker (Choice.boundedEnumHasChoiceInputVal show)
    { title: "Meridiem"
    , values: upFromIncluding (bottom :: Meridiem)
    })
  unit
  (HE.input $ \(NotifyChange n) -> Update $ (F.toSetter cmd (fromEnum n)))
renderCommand cmd = toAlt $ do
  conf <- commandToConfig cmd
  pure $ HH.slot' cpNum cmd (Num.picker Num.intHasNumberInputVal conf) unit
    (HE.input $ \(NotifyChange n) -> Update $ \t -> n >>= (_ `F.toSetter cmd` t))

commandToConfig :: F.Command -> Maybe {title :: String, range :: Range Int }
commandToConfig F.Hours24               = Just {title: "Hours", range: (bottomTop :: Range Hour) <#> fromEnum }
commandToConfig F.Hours12               = Just {title: "Hours", range: (bottomTop :: Range Hour12) <#> fromEnum }
commandToConfig F.Meridiem              = Just {title: "Meridiem", range: (bottomTop :: Range Meridiem) <#> fromEnum }
commandToConfig F.MinutesTwoDigits      = Just {title: "Minutes", range: (bottomTop :: Range Minute) <#> fromEnum }
commandToConfig F.Minutes               = Just {title: "Minutes", range: (bottomTop :: Range Minute) <#> fromEnum }
commandToConfig F.SecondsTwoDigits      = Just {title: "Seconds", range: (bottomTop :: Range Second) <#> fromEnum }
commandToConfig F.Seconds               = Just {title: "Seconds", range: (bottomTop :: Range Second) <#> fromEnum }
commandToConfig F.Milliseconds          = Just {title: "Milliseconds", range: (bottomTop :: Range Millisecond) <#> fromEnum }
commandToConfig F.MillisecondsTwoDigits = Just {title: "Milliseconds", range: (bottomTop :: Range Millisecond2) <#> fromEnum }
commandToConfig F.MillisecondsShort     = Just {title: "Milliseconds", range: (bottomTop :: Range Millisecond1) <#> fromEnum }
commandToConfig (F.Placeholder _)       = Nothing



evalTime ∷ ∀ m . TimeQuery ~> DSL m
evalTime (Update update next) = do
  s <- H.get
  nextTime <- map (steper' s.time InvalidTime) $ case s.time of
    Just (Right time) -> pure $ update time
    _  -> buildTime
  H.modify _{ time = nextTime }
  when (nextTime /= s.time) $ H.raise (NotifyChange nextTime)
  pure next


buildTime :: ∀ m. DSL m (Maybe Time)
buildTime = do
  {format} <- H.get
  mbKleisliEndo <- for (sort $ unwrap format) mkKleisli
  pure $ map fold (sequence mbKleisliEndo) >>= \(Join (Star f)) -> f bottom
  where
  mkKleisli (F.Placeholder _) = pure $ Just $ mempty
  mkKleisli cmd@F.Meridiem = do
    num <- H.query' cpChoice unit $ H.request (left <<< GetValue)
    pure $ num <#> \n -> Join $ Star $ \t -> F.toSetter cmd (fromEnum n) t
  mkKleisli cmd = do
    num <- H.query' cpNum cmd $ H.request (left <<< GetValue)
    pure $ join num <#> \n -> Join $ Star $ \t -> F.toSetter cmd n t


evalPicker ∷ ∀ m . QueryIn ~> DSL m
evalPicker (ResetError next) = do
  H.modify _{ time = Nothing }
  pure next
evalPicker (Base (SetValue time next)) = do
  {format} <- H.get
  propagateChange format time
  H.modify _{ time = time }
  pure $ next unit
evalPicker (Base (GetValue next)) = H.gets _.time <#> next

propagateChange :: ∀ m . F.Format -> Input -> DSL m Unit
propagateChange format time = do
  map (mustBeMounted <<< fold) $ for (unwrap format) change
  where
  change :: F.Command -> DSL m (Maybe Unit)
  change (F.Placeholder _ ) = pure (Just unit)
  change F.Meridiem = do
    res <- H.query' cpChoice unit $ H.request $ left <<< SetValue m
    pure $ res >>= case _ of
      Just Choice.ValueIsNotInValues -> Nothing
      Nothing -> Just unit
    where
    m :: Meridiem
    m = (value time >>= F.toGetter F.Meridiem >>= toEnum) # maybe bottom id
  change cmd = do
    let n = value time >>= F.toGetter cmd
    H.query' cpNum cmd $ H.request $ left <<< SetValue n
