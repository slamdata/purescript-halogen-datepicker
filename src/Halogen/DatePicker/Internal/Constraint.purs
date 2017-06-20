module Halogen.Datapicker.Internal.Constraint where

import Prelude

import Control.Monad.State (State, get, put, execState)
import Data.Foldable (class Foldable, any, null, length, for_)
import Data.List as List
import Data.Validation.Semigroup (V, invalid, unV)


data Error 
  = ContainsInvalidValue String
  | ShouldBeNonEmpty
  | ShouldBeSorted
  | UsageCountShouldBeNoneOrOne { count :: Int, alowed:: String }
  | UsageCountShouldBeNoneOrAll { count :: Int, alowed:: String }

showError :: Error -> String 
showError = case _ of
  ContainsInvalidValue val ->
    "Contains value: " <> val <> " which is not allowed"
  ShouldBeNonEmpty ->
    "Input must contain values"
  ShouldBeSorted ->
    "Input Must be sorted"
  UsageCountShouldBeNoneOrOne { count, alowed } ->
    "Usage count (" <> show count <> ") for allowed elements (" <> alowed <> ") must be 0 or 1"
  UsageCountShouldBeNoneOrAll { count, alowed } ->
    "Usage count (" <> show count <> ") for allowed elements (" <> alowed <> ") must be 0 or all"

type Constraint a = a -> V (Array Error) Unit

runConstraint :: ∀ a g. Constraint (g a) -> g a -> Array String
runConstraint f a = unV (map showError) (const []) $ f a

allowedValues :: ∀ g a. Foldable g => (a -> String) -> Array (EqPred a) -> Constraint (g a)
allowedValues showVal as as' = for_ as' \a -> unless
  (matchesAny a as) 
  (invalid [ContainsInvalidValue $ showVal a])

notEmpty :: ∀ f a. Foldable f => Constraint (f a)
notEmpty as = when (null as) (invalid [ShouldBeNonEmpty])

data Sorting = Increasing | Decreasing

sorted :: ∀ f a. Ord a => Foldable f => Sorting -> Constraint (f a)
sorted sorting as = unless isSorted (invalid [ShouldBeSorted])
  where
  isSorted = case sorting of
    Increasing -> List.reverse asList == List.sort asList
    Decreasing -> asList == List.sort asList
  asList = List.fromFoldable as


allowNoneOrOne :: ∀ g a. Foldable g => Array (EqPred a) -> Constraint (g a)
allowNoneOrOne as = usageCount as >>> \c -> when 
  (c > 1)
  (invalid [UsageCountShouldBeNoneOrOne { count: c,  alowed: show as }])

allowNoneOrAll :: ∀ f a. Foldable f => Array (EqPred a) -> Constraint (f a)
allowNoneOrAll as = usageCount as >>> \c -> when
  (c /= 0 && c /= length as) 
  (invalid [UsageCountShouldBeNoneOrOne { count: c,  alowed: show as }])

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
