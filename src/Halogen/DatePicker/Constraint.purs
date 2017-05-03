module Halogen.Datapicker.Constraint where

import Prelude
import Data.Validation.Semigroup (V, invalid, unV)
import Data.Foldable (class Foldable, elem, null, length, for_)
import Control.Monad.State (State, get, put, execState)


type Constraint g a = g a -> V (Array String) Unit

runConstraint :: ∀ a g. Constraint g a -> g a -> Array String
runConstraint f a = unV id (const []) $ f a

allowedValues :: ∀ g f a. Eq a => Show a => Foldable f => Foldable g => f a -> Constraint g a
allowedValues as as' = for_ as' \a ->
  if elem a as
  then pure unit
  else invalid ["contains value: " <> (show a) <> " which is not allowed"]

notEmpty :: ∀ g a. Foldable g => Constraint g a
notEmpty as = if null as then invalid ["input must contain values"] else pure unit

allowNoneOrOne :: ∀ g f a. Eq a => Show (f a) => Foldable f => Foldable g => f a -> Constraint g a
allowNoneOrOne as = f <<< usageCount as
  where
  f c | c > 1 = invalid ["usage count (" <> show c <> ") for allowed ellements (" <> (show as) <> ") must be 0 or 1"]
  f _ = pure unit

allowNoneOrAll :: ∀ g f a. Eq a => Show (f a) => Foldable f => Foldable g => f a -> Constraint g a
allowNoneOrAll as = f <<< usageCount as
  where
  f c | c /= 0 && c /= length as = invalid ["usage count (" <> show c <> ") for allowed ellements (" <> (show as) <> ") must be 0 or all"]
  f _ = pure unit


usageCount :: ∀ g f a. Eq a => Foldable f => Foldable g => f a -> g a -> Int
usageCount as as' = for_ as' incState `execState` 0
  where
  incState :: a -> State Int Unit
  incState a = get >>= \count -> if elem a as then put (count + 1) else pure unit
