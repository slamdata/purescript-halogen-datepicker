module Halogen.Datepicker.Internal.Utils where

import Prelude

import Control.Alternative (class Alternative, empty)
import Control.MonadPlus (guard)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.Datepicker.Component.Types (PickerMessage(..), PickerValue, isInvalid)
import Halogen.HTML (ClassName(..))
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM (HalogenM)
import Partial.Unsafe (unsafePartialBecause)

mustBeMounted ∷ ∀ a. Maybe a → a
mustBeMounted a = unsafePartialBecause "children must be mounted" (fromJust a)

steper' ∷ ∀ e a. PickerValue e a → e → Either Boolean a → PickerValue e a
steper' old err = steper old <<< lmap (_ `Tuple` err)

steper ∷ ∀ e a. PickerValue e a → Either (Tuple Boolean e) a → PickerValue e a
steper old new = case old, new of
  _, Right x → Just (Right x)
  Just _, Left (Tuple _ err) → Just (Left err)
  -- `true` indicates if we want to force state change to "invalid"
  Nothing, Left (Tuple true err) → Just (Left err)
  Nothing, Left _ → Nothing


pickerProps :: ∀ e a r z. PickerValue e a → Array (HP.IProp ( "class" :: String | z ) r )
pickerProps val = [HP.classes classes]
  where
  classes = [ClassName "Picker"] <> (guard (isInvalid val) $> ClassName "Picker--invalid")

componentProps :: forall r z. Array (HP.IProp ( "class" :: String | z ) r )
componentProps = [HP.classes [ClassName "Picker-component"]]

asRight ∷ ∀ e a f. Alternative f => Either e a → f a
asRight = either (const empty) pure

asLeft ∷ ∀ e a f. Alternative f => Either e a → f e
asLeft = either pure (const empty)

moveStateTo :: forall m o p f a. Eq a => a -> a -> HalogenM a f p o (PickerMessage a) m Unit
moveStateTo old new = H.put new *> unless (new == old) (H.raise $ NotifyChange new)
