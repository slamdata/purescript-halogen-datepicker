module Halogen.Datapicker.Component.Types where

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromJust, isNothing)
import Data.Unit (Unit)
import Partial.Unsafe (unsafePartialBecause)

data PickerQuery err val next
  = GetValue (val -> next)
  | SetValue val (err -> next)

data PickerMessage val
  = NotifyChange val

mustBeMounted :: Maybe Unit -> Unit
mustBeMounted = unsafePartialBecause "children must be mounted" fromJust

type PickerValue e a = Maybe (Either e a)

isIdle :: ∀ e a. PickerValue e a -> Boolean
isIdle = isNothing

isInvalid :: ∀ e a. PickerValue e a -> Boolean
isInvalid (Just (Left _)) = true
isInvalid _ = false

isValid :: ∀ e a. PickerValue e a -> Boolean
isValid (Just (Right _)) = true
isValid _ = false

value :: ∀ e a. PickerValue e a -> Maybe a
value (Just (Right x)) = Just x
value _ = Nothing

error :: ∀ e a. PickerValue e a -> Maybe e
error (Just (Left x)) = Just x
error _ = Nothing
