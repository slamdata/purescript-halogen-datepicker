module Halogen.Datepicker.Component.Types where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust)

data PickerQuery err val next
  = ResetError next
  | Base (BasePickerQuery err val next)

data BasePickerQuery err val next
  = GetValue (val → next)
  | SetValue val (err → next)

data PickerMessage val
  = NotifyChange val


type PickerValue e a = Maybe (Either e a)

value ∷ ∀ e a. PickerValue e a → Maybe a
value (Just (Right x)) = Just x
value _ = Nothing

error ∷ ∀ e a. PickerValue e a → Maybe e
error (Just (Left x)) = Just x
error _ = Nothing


isInvalid ∷ ∀ e a. PickerValue e a → Boolean
isInvalid = error >>> isJust

isValid ∷ ∀ e a. PickerValue e a → Boolean
isValid = value >>> isJust
