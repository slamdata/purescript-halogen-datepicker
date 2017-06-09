module Halogen.Datapicker.Component.Types where

import Prelude
import Control.Alternative (class Alternative, empty)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromJust, isNothing, maybe)
import Partial.Unsafe (unsafePartialBecause)

data PickerQuery err val next
  = GetValue (val -> next)
  | SetValue val (err -> next)

data PickerMessage val
  = NotifyChange val

mustBeMounted :: Maybe Unit -> Unit
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

stepPickerValue :: ∀ e a m . Monad m => (Maybe a -> m (Either e a)) -> PickerValue e a -> m (PickerValue e a)
stepPickerValue step old = do
  newVal <- step (value old)
  pure case old, newVal of
    _, Right x -> Just (Right x)
    Just _, Left err -> Just (Left err)
    Nothing, Left _ -> Nothing

stepPickerValue' :: ∀ m e a. Monad m => e -> (Maybe a -> m (Maybe a)) -> PickerValue e a -> m (PickerValue e a)
stepPickerValue' err step = stepPickerValue (\mbI -> step mbI <#> maybe (Left err) Right)


toAlt :: ∀ f. Alternative f => Maybe ~> f
toAlt (Just a) = pure a
toAlt Nothing = empty
