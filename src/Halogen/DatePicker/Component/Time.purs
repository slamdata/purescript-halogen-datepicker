module Halogen.Datapicker.Component.Time where

import Prelude
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.Datapicker.Component.Internal.Choice as Choice
import Halogen.Datapicker.Component.Internal.Num as Num
import Halogen.Datapicker.Component.Time.Format as F
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Control.MonadPlus (guard)
import Data.Bifunctor (bimap)
import Data.DateTime (Hour, Millisecond, Minute, Second)
import Data.Either (either)
import Data.Either.Nested (Either2)
import Data.Enum (class Enum, fromEnum, toEnum, upFrom)
import Data.Foldable (fold, foldMap)
import Data.Functor.Coproduct (Coproduct, coproduct, right, left)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (sort)
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Monoid (mempty)
import Data.Newtype (unwrap)
import Data.NonEmpty (NonEmpty(..))
import Data.Profunctor.Join (Join(..))
import Data.Profunctor.Star (Star(..))
import Data.Time (Time)
import Data.Traversable (for, sequence)
import Data.Unfoldable (class Unfoldable)
import Halogen.Datapicker.Component.Internal.Elements (textElement)
import Halogen.Datapicker.Component.Internal.Enums (Hour12, Meridiem, Millisecond1, Millisecond2)
import Halogen.Datapicker.Component.Internal.Range (Range, bottomTop)
import Halogen.Datapicker.Component.Types (PickerMessage(..), PickerQuery(..), PickerValue, isInvalid, mustBeMounted, stepPickerValue')

data TimeQuery a = UpdateCommand (Time -> Maybe Time) a
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
render s =
  HH.ul
    [ HP.classes $
      [HH.ClassName "Picker"]
      <> (guard (isInvalid s.time) $> HH.ClassName "Picker--invalid")
    ]
    (foldMap (pure <<< f) (unwrap s.format))
  where
  f cmd = HH.li [HP.classes [HH.ClassName "Picker-component"]] [ renderCommand cmd ]

onMb :: ∀ a b. (a -> b -> Maybe b) -> Maybe a -> b -> Maybe b
onMb f a b = a >>= \a' -> f a' b

upFromIncluding :: ∀ a u. Enum a => Unfoldable u => a -> NonEmpty u a
upFromIncluding x = NonEmpty x $ upFrom x

renderCommand :: ∀ m. F.Command -> HTML m
renderCommand (F.Placeholder str) = textElement { text: str}
renderCommand cmd@F.Meridiem = HH.slot'
  cpChoice
  unit
  (Choice.picker Choice.boundedEnumHasChoiceInputVal
    { title: "Meridiem"
    , values: upFromIncluding (bottom :: Meridiem)
    })
  unit
  (HE.input $ \(NotifyChange n) -> UpdateCommand $ (F.toSetter cmd (fromEnum n)))
renderCommand cmd = HH.slot'
  cpNum
  cmd
  (Num.picker Num.intHasNumberInputVal $ commandToConfig cmd)
  unit
  (HE.input $ \(NotifyChange n) -> UpdateCommand $ \t -> n >>= (_ `F.toSetter cmd` t))

commandToConfig :: F.Command -> {title :: String, range :: Range Int }
commandToConfig F.Hours24               = {title: "Hours", range: (bottomTop :: Range Hour) <#> fromEnum }
commandToConfig F.Hours12               = {title: "Hours", range: (bottomTop :: Range Hour12) <#> fromEnum }
commandToConfig F.Meridiem              = {title: "Meridiem", range: (bottomTop :: Range Meridiem) <#> fromEnum }
commandToConfig F.MinutesTwoDigits      = {title: "Minutes", range: (bottomTop :: Range Minute) <#> fromEnum }
commandToConfig F.Minutes               = {title: "Minutes", range: (bottomTop :: Range Minute) <#> fromEnum }
commandToConfig F.SecondsTwoDigits      = {title: "Seconds", range: (bottomTop :: Range Second) <#> fromEnum }
commandToConfig F.Seconds               = {title: "Seconds", range: (bottomTop :: Range Second) <#> fromEnum }
commandToConfig F.Milliseconds          = {title: "Milliseconds", range: (bottomTop :: Range Millisecond) <#> fromEnum }
commandToConfig F.MillisecondsTwoDigits = {title: "Milliseconds", range: (bottomTop :: Range Millisecond2) <#> fromEnum }
commandToConfig F.MillisecondsShort     = {title: "Milliseconds", range: (bottomTop :: Range Millisecond1) <#> fromEnum }
commandToConfig (F.Placeholder _)       = {title: "NO TITLE HERE", range: (bottomTop :: Range Char) <#> fromEnum }



evalTime ∷ ∀ m . TimeQuery ~> DSL m
evalTime (UpdateCommand update next) = do
  s <- H.get
  nextTime <- stepPickerValue'
    InvalidTime
    (maybe buildTime $ update >>> pure)
    s.time
  H.modify _{ time = nextTime }
  when (nextTime /= s.time) $ H.raise (NotifyChange nextTime)
  pure next

-- TODO use normal `foldl <=< pure` instead :/
-- newtype KleisliEndo m a = KleisliEndo (a -> m a)
--
-- derive instance newtypeKleisliEndo :: Newtype (KleisliEndo m a) _
--
-- instance semigroupKleisliEndo :: Bind m => Semigroup (KleisliEndo m a) where
--   append (KleisliEndo f) (KleisliEndo g) = KleisliEndo (f <=< g)
--
-- instance monoidKleisliEndo :: Monad m => Monoid (KleisliEndo m a) where
--   mempty = KleisliEndo pure


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
evalPicker (SetValue time next) = do
  {format} <- H.get
  propagateChange format time
  H.modify _{ time = time }
  pure $ next unit
evalPicker (GetValue next) = H.gets _.time <#> next

propagateChange :: ∀ m . F.Format -> Input -> DSL m Unit
propagateChange format duration = do
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
    m = (duration >>= either (const Nothing) (F.toGetter F.Meridiem) >>= toEnum) # maybe bottom id
  change cmd = do
    let n = (duration >>= either (const Nothing) (F.toGetter cmd)) :: Maybe Int
    H.query' cpNum cmd $ H.request $ left <<< SetValue n
