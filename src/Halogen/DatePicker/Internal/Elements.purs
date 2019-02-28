module Halogen.Datepicker.Internal.Elements where

import Prelude

import Data.Enum (class BoundedEnum, fromEnum, toEnum)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.NonEmpty (NonEmpty)
import Data.Symbol (class IsSymbol, SProxy)
import Halogen as H
import Halogen.Datepicker.Config (Config(..))
import Halogen.Datepicker.Internal.Choice as Choice
import Halogen.Datepicker.Internal.Num as Num
import Halogen.Datepicker.Internal.Range (Range)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Prim.Row as Row

textElement ∷ ∀ p i. Config → {text ∷ String} → HH.HTML p i
textElement (Config {placeholder}) {text} = HH.span [HP.classes placeholder] [HH.text text]

type PreNumConfig a = {title ∷ String, placeholder ∷ String, range ∷ Range a}
type PreChoiceConfig a = {title ∷ String, values ∷ NonEmpty Array a}

toNumConf
  ∷ ∀ a
  . Config
  → PreNumConfig a
  → Num.Config a
toNumConf (Config {input, inputInvalid, inputLength}) ({title, placeholder, range}) =
  {title, placeholder, range, root: input, rootInvalid: inputInvalid, rootLength: inputLength }

type OptionalUpdate a = a → Maybe a

renderNum
  ∷ ∀ m sym cmd px ps queryVal
  . Row.Cons sym ((Num.Slot Int) cmd) px ps
  ⇒ IsSymbol sym
  ⇒ Ord cmd
  ⇒ SProxy sym
  → (cmd → Int → OptionalUpdate queryVal)
  → cmd
  → Config
  → PreNumConfig Int
  → H.ComponentHTML (OptionalUpdate queryVal) ps m
renderNum sym toSetter cmd mainConf preConf =
  let
    conf = toNumConf mainConf preConf
  in
    HH.slot sym cmd
      (Num.picker Num.intHasNumberInputVal conf) unit
      (\n → Just \t → n >>= (_ `toSetter cmd` t))

toChoiceConf
  ∷ ∀ a
  . Config
  → PreChoiceConfig a
  → Choice.Config a
toChoiceConf (Config {choice}) ({title, values}) =
  {title, values, root: choice }

renderChoice
  ∷ ∀ a m sym cmd px ps queryVal
  . BoundedEnum a
  ⇒ Show a
  ⇒ Row.Cons sym ((Choice.Slot (Maybe Int)) cmd) px ps
  ⇒ IsSymbol sym
  ⇒ Ord cmd
  ⇒ SProxy sym
  → (cmd → Int → OptionalUpdate queryVal)
  → cmd
  → Config
  → PreChoiceConfig (Maybe a)
  → H.ComponentHTML (OptionalUpdate queryVal) ps m
renderChoice cpChoice toSetter cmd mainConf preConf =
  let
    conf = toChoiceConf mainConf preConf
    emptyVal = case mainConf of
      Config {choiceEmptyTitle} → fromMaybe "" $ unwrap choiceEmptyTitle
  in
    HH.slot cpChoice cmd
      ( Choice.picker
        (Choice.maybeIntHasChoiceInputVal \n → ((n >>= toEnum) ∷ Maybe a) # maybe emptyVal show)
        (conf{values = conf.values <#> map fromEnum})
      )
      unit
      (\n → Just \t → n >>= (_ `toSetter cmd` t))
