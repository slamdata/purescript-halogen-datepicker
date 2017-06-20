module Halogen.Datepicker.Internal.Utils where

import Prelude

import Control.MonadPlus (guard)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Halogen.HTML (ClassName(..))
import Partial.Unsafe (unsafePartialBecause)
import Halogen.Datepicker.Component.Types (PickerValue)

mustBeMounted ∷ ∀a. Maybe a -> a
mustBeMounted a = unsafePartialBecause "children must be mounted" (fromJust a)

steper' ∷ ∀ e a. PickerValue e a -> e -> Either Boolean a -> PickerValue e a
steper' old err = steper old <<< lmap (_ `Tuple` err)

steper ∷ ∀ e a. PickerValue e a -> Either (Tuple Boolean e) a -> PickerValue e a
steper old new = case old, new of
  _, Right x -> Just (Right x)
  Just _, Left (Tuple _ err) -> Just (Left err)
  -- `true` indicates if we want to force state change to "invalid"
  Nothing, Left (Tuple true err) -> Just (Left err)
  Nothing, Left _ -> Nothing

pickerClasses ∷ ∀ e a. PickerValue e a -> Array ClassName
pickerClasses val = [ClassName "Picker"] <> (guard (isInvalid val) $> ClassName "Picker--invalid")
  where
  isInvalid ∷ PickerValue e a -> Boolean
  isInvalid (Just (Left _)) = true
  isInvalid _ = false
