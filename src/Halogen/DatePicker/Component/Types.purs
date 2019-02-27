module Halogen.Datepicker.Component.Types where

import Prelude

import Data.Either (Either(..))
import Data.Functor.Coproduct (Coproduct, left)
import Data.Maybe (Maybe(..), isJust)
import Halogen (request, tell)

data PickerQuery err val next
  = ResetError next
  | Base (BasePickerQuery err val next)

data BasePickerQuery err val next
  = GetValue (val → next)
  | SetValue val (err → next)

type PickerValue e a = Maybe (Either e a)

getValue ∷ ∀ val err r. Coproduct (PickerQuery err val) r val
getValue = request $ left <<< Base <<< GetValue

setValue ∷ ∀ val err r. val -> Coproduct (PickerQuery err val) r err
setValue val = request $ left <<< (Base <<< SetValue val)

resetError ∷ ∀ val err r. Coproduct (PickerQuery err val) r Unit
resetError = tell $ left <<< ResetError

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
