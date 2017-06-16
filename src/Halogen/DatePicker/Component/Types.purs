module Halogen.Datapicker.Component.Types where

import Prelude

import Control.Alternative (class Alternative, empty)
import Control.MonadPlus (guard)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromJust, isNothing, maybe)
import Data.Tuple (Tuple(..))
import Halogen.HTML (ClassName(..))
import Partial.Unsafe (unsafePartialBecause)

data PickerQuery err val next
  = ResetError next
  | Base (BasePickerQuery err val next)

data BasePickerQuery err val next
  = GetValue (val -> next)
  | SetValue val (err -> next)

data PickerMessage val
  = NotifyChange val

mustBeMounted :: ∀a. Maybe a -> a
mustBeMounted a = unsafePartialBecause "children must be mounted" (fromJust a)

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

steperMaybe :: ∀ e a. PickerValue e a -> e -> Maybe a -> PickerValue e a
steperMaybe old err = steper old <<< maybe (Left err) Right

steperMaybe' :: ∀ e a. PickerValue e a -> e -> Either Boolean a -> PickerValue e a
steperMaybe' old err = steper' old <<< lmap (_ `Tuple` err)

steper' :: ∀ e a. PickerValue e a -> Either (Tuple Boolean e) a -> PickerValue e a
steper' old new = case old, new of
  _, Right x -> Just (Right x)
  Just _, Left (Tuple _ err) -> Just (Left err)
  -- `true` indicates if we want to force state change to "invalid"
  Nothing, Left (Tuple true err) -> Just (Left err)
  Nothing, Left _ -> Nothing

steper :: ∀ e a. PickerValue e a -> Either e a -> PickerValue e a
steper old new = steper' old (lmap (Tuple false) new)

toAlt :: ∀ f. Alternative f => Maybe ~> f
toAlt (Just a) = pure a
toAlt Nothing = empty

pickerClasses :: forall e a. PickerValue e a -> Array ClassName
pickerClasses val = [ClassName "Picker"] <> (guard (isInvalid val) $> ClassName "Picker--invalid")
