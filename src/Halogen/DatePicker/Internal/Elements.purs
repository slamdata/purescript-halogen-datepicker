module Halogen.Datepicker.Internal.Elements where

import Prelude

import Data.Enum (class BoundedEnum, fromEnum, toEnum)
import Data.Maybe (Maybe, maybe)
import Data.NonEmpty (NonEmpty)
import Halogen as H
import Halogen.Component.ChildPath (ChildPath)
import Halogen.Datepicker.Component.Types (PickerMessage(..))
import Halogen.Datepicker.Config (Config(..))
import Halogen.Datepicker.Internal.Choice as Choice
import Halogen.Datepicker.Internal.Num as Num
import Halogen.Datepicker.Internal.Range (Range)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query (Action)

textElement ∷ ∀ p i. Config → {text ∷ String} → H.HTML p i
textElement (Config {placeholder}) {text} = HH.span [HP.classes placeholder] [HH.text text]


type PreNumConfig a = {title ∷ String, placeholder ∷ String, range ∷ Range a}
type PreChoiceConfig a = {title ∷ String, values ∷ NonEmpty Array a}

toNumConf ∷
  ∀ a. Config
  → PreNumConfig a
  → Num.Config a
toNumConf (Config {input, inputInvalid, inputLength}) ({title, placeholder, range}) =
  {title, placeholder, range, root: input, rootInvalid: inputInvalid, rootLength: inputLength }

type OptionalUpdate a = a -> Maybe a

renderNum :: ∀ m slot cmd childQuery parentQuery queryVal
  . ChildPath (Num.Query Int) childQuery cmd slot
  → (OptionalUpdate queryVal → Action parentQuery)
  → (cmd → Int → OptionalUpdate queryVal)
  → cmd
  → Config
  → PreNumConfig Int
  → H.ParentHTML parentQuery childQuery slot m
renderNum cpNum update toSetter cmd mainConf preConf =
  let
    conf = toNumConf mainConf preConf
  in
    HH.slot' cpNum cmd
      (Num.picker Num.intHasNumberInputVal conf) unit
      (HE.input $ \(NotifyChange n) → update $ \t → n >>= (_ `toSetter cmd` t))

toChoiceConf ∷
  ∀ a. Config
  → PreChoiceConfig a
  → Choice.Config a
toChoiceConf (Config {choice}) ({title, values}) =
  {title, values, root: choice }

renderChoice :: ∀ a m slot cmd childQuery parentQuery queryVal
  . BoundedEnum a
  ⇒ Show a
  ⇒ ChildPath (Choice.Query (Maybe Int)) childQuery cmd slot
  → (OptionalUpdate queryVal → Action parentQuery)
  → (cmd → Int → OptionalUpdate queryVal)
  → cmd
  → Config
  → PreChoiceConfig (Maybe a)
  → H.ParentHTML parentQuery childQuery slot m
renderChoice cpChoice update toSetter cmd mainConf preConf =
  let
    conf = toChoiceConf mainConf preConf
  in
    HH.slot' cpChoice cmd
      ( Choice.picker
        (Choice.maybeIntHasChoiceInputVal \n → ((n >>= toEnum) ∷ Maybe a) # maybe "--" show)
        (conf{values = conf.values <#> map fromEnum})
      )
      unit
      (HE.input $ \(NotifyChange n) → update $ \t → n >>= (_ `toSetter cmd` t))
