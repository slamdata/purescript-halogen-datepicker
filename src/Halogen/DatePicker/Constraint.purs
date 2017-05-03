module Halogen.Datapicker.Constraint where

import Prelude
import Data.Either(Either(..))
import Data.Foldable (class Foldable, elem, null, length, for_)
import Control.Monad.State (State, get, put, execState)


-- NOTE Validation could be used instead of Either
type Constraint g a = g a -> Either String Unit

allowedValues :: ∀ g f a. Eq a => Show a => Foldable f => Foldable g => f a -> Constraint g a
allowedValues as as' = for_ as' \a ->
  if elem a as
  then Right unit
  else Left ("contains value: " <> (show a) <> " which is not allowed")

notEmpty :: ∀ g a. Foldable g => Constraint g a
notEmpty as = if null as then Left "input must contain values" else Right unit

alloweNoneOrOne :: ∀ g f a. Eq a => Show (f a) => Foldable f => Foldable g => f a -> Constraint g a
alloweNoneOrOne as = f <<< usageCount as
  where
  f c | c > 1 = Left $ "usage count (" <> show c <> ") for allowed ellements (" <> (show as) <> ") must be 0 or 1"
  f _ = Right unit

alloweNoneOrAll :: ∀ g f a. Eq a => Show (f a) => Foldable f => Foldable g => f a -> Constraint g a
alloweNoneOrAll as = f <<< usageCount as
  where
  f c | c /= 0 && c /= length as = Left $ "usage count (" <> show c <> ") for allowed ellements (" <> (show as) <> ") must be 0 or all"
  f _ = Right unit


usageCount :: ∀ g f a. Eq a => Foldable f => Foldable g => f a -> g a -> Int
usageCount as as' = for_ as' incState `execState` 0
  where
  incState :: a -> State Int Unit
  incState a = get >>= \count -> if elem a as then put (count + 1) else pure unit
