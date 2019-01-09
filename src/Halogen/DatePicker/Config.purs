module Halogen.Datepicker.Config where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Monoid (genericMempty)
import Data.Generic.Rep.Semigroup (genericAppend)
import Data.Maybe (Maybe(..))
import Data.Maybe.Last (Last(..))
import Data.Newtype (class Newtype)
import Halogen.HTML (ClassName(..))

newtype Config = Config
  { root ∷ Array ClassName
  , rootInvalid ∷ Array ClassName
  , component ∷ Array ClassName
  , placeholder ∷ Array ClassName
  , choice ∷ Array ClassName
  , choiceEmptyTitle ∷ Last String
  , input ∷ Array ClassName
  , inputInvalid ∷ Array ClassName
  , inputLength ∷ Int → Array ClassName
  }

defaultConfig ∷ Config
defaultConfig = Config
  { root: [ClassName "Picker"]
  , rootInvalid: [ClassName "Picker--invalid"]
  , component: [ClassName "Picker-component"]
  , placeholder: [ClassName "Picker-placeholder"]
  , choice: [ClassName "Picker-input"]
  , choiceEmptyTitle: Last $ Just "--"
  , input: [ClassName "Picker-input"]
  , inputInvalid: [ClassName "Picker-input--invalid"]
  , inputLength: \n → [ ClassName $ "Picker-input--length-" <> show n]
  }

derive instance configGeneric :: Generic Config _
derive instance configNewtype :: Newtype Config _

instance configSemigroup :: Semigroup Config where
  append = genericAppend

instance configMonoid :: Monoid Config where
  mempty = genericMempty
