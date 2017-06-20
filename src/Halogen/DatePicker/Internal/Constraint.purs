module Halogen.Datapicker.Internal.Constraint where

import Prelude

import Control.Monad.State (State, get, put, execState)
import Data.Foldable (class Foldable, any, null, length, for_)
import Data.List as List
import Data.Validation.Semigroup (V, invalid, unV)

type Constraint a = a -> V (Array String) Unit

runConstraint :: ∀ a g. Constraint (g a) -> g a -> Array String
runConstraint f a = unV id (const []) $ f a

allowedValues :: ∀ g a. Foldable g => (a -> String) -> Array (EqPred a) -> Constraint (g a)
allowedValues showVal as as' = for_ as' \a -> unless
  (matchesAny a as) 
  (invalid ["Contains value: " <> (showVal a) <> " which is not allowed"])

notEmpty :: ∀ f a. Foldable f => Constraint (f a)
notEmpty as = when (null as) (invalid ["Input must contain values"])

data Sorting = Increasing | Decreasing

sorted :: ∀ f a. Ord a => Foldable f => Sorting -> Constraint (f a)
sorted sorting as = unless isSorted (invalid ["Input Must be sorted"])
  where
  isSorted = case sorting of
    Increasing -> List.reverse asList == List.sort asList
    Decreasing -> asList == List.sort asList
  asList = List.fromFoldable as


allowNoneOrOne :: ∀ g a. Foldable g => Array (EqPred a) -> Constraint (g a)
allowNoneOrOne as = f <<< usageCount as
  where
  f c = when (c > 1) $ invalid ["Usage count (" <> show c <> ") for allowed elements (" <> (show as) <> ") must be 0 or 1"]

allowNoneOrAll :: ∀ f a. Foldable f => Array (EqPred a) -> Constraint (f a)
allowNoneOrAll as = f <<< usageCount as
  where
  f c  = when (c /= 0 && c /= length as) $ invalid ["Usage count (" <> show c <> ") for allowed elements (" <> (show as) <> ") must be 0 or all"]

matchesAny :: ∀ a . a -> Array (EqPred a) -> Boolean
matchesAny = any <<< equals

usageCount :: ∀ f a. Foldable f => Array (EqPred a) -> f a -> Int
usageCount as as' = for_ as' incState `execState` 0
  where
  incState :: a -> State Int Unit
  incState a = get >>= \count -> when (matchesAny a as) (put $ count + 1)


data EqPred a = EqPred String (a -> Boolean)

instance predShow :: Show (EqPred a) where
  show (EqPred str _)= str

equals :: ∀ a . a -> EqPred a -> Boolean
equals a (EqPred _ p) = p a

reShow :: ∀ a . Eq a => (a -> String) -> a -> EqPred a
reShow f a = EqPred (f a) (_ == a)

pred :: ∀ a . Eq a => Show a => a -> EqPred a
pred = reShow show
