module Halogen.Datapicker.Component.Time where

import Prelude
import Halogen as H
import Halogen.Datapicker.Component.Internal.Num as N
import Halogen.Datapicker.Component.Time.Format as F
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Control.MonadPlus (guard)
import Data.Bifunctor (bimap)
import Data.DateTime (Hour, Millisecond, Minute, Second)
import Data.Either (either)
import Data.Enum (fromEnum)
import Data.Foldable (fold, foldMap)
import Data.Functor.Coproduct (Coproduct, coproduct, right, left)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, unwrap)
import Data.Time (Time)
import Data.Traversable (for, sequence)
import Halogen.Datapicker.Component.Internal.Elements (enumElement, textElement)
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

type Slot = F.Command
type ChildQuery = N.Query Int

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

renderCommand :: ∀ m. F.Command -> HTML m
-- TODO fix once we fix choiceElement
-- renderCommand cmd@F.Meridiem      = choiceElement (UpdateCommand cmd) { title: "Meridiem" } (meridiem t)
renderCommand cmd@F.Meridiem      = textElement { text: "foo"}
renderCommand (F.Placeholder str) = textElement { text: str}
renderCommand cmd                 = enumElement cmd UpdateCommand (commandToConfig cmd) (onMb $ F.toSetter cmd)

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
    case _ of
      Nothing -> buildTime
      Just val -> pure (update val)
    s.time
  H.modify _{ time = nextTime }
  when (nextTime /= s.time) $ H.raise (NotifyChange nextTime)
  pure next

-- TODO use normal `foldl <=< pure` instead :/
newtype KleisliEndo m a = KleisliEndo (a -> m a)

derive instance newtypeKleisliEndo :: Newtype (KleisliEndo m a) _

instance semigroupKleisliEndo :: Bind m => Semigroup (KleisliEndo m a) where
  append (KleisliEndo f) (KleisliEndo g) = KleisliEndo (f <=< g)

instance monoidKleisliEndo :: Monad m => Monoid (KleisliEndo m a) where
  mempty = KleisliEndo pure


buildTime :: ∀ m. DSL m (Maybe Time)
buildTime = do
  {format} <- H.get
  mbKleisliEndo <- for (unwrap format) mkKleisli
  pure case map fold $ sequence mbKleisliEndo of
   Just (KleisliEndo f) -> f bottom
   _ -> Nothing
  where
  mkKleisli (F.Placeholder _) = pure $ Just $ mempty
  mkKleisli cmd = do
    num <- H.query cmd $ H.request (left <<< GetValue)
    pure case num of
      Just (Just n) -> Just $ KleisliEndo $ \t -> F.toSetter cmd n t
      _ -> Nothing



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
  change (F.Placeholder _ ) = pure (Just unit)
  change cmd = do
    let n = (duration >>= either (const Nothing) (F.toGetter cmd)) :: Maybe Int
    H.query cmd $ H.request $ left <<< SetValue n
