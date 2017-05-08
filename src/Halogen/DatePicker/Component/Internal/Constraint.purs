module Halogen.Datapicker.Component.Internal.Constraint where

import Prelude
import Data.Validation.Semigroup (V, invalid, unV)
import Data.Foldable (class Foldable, any, null, length, for_)
import Control.Monad.State (State, get, put, execState)


type Constraint a = a -> V (Array String) Unit

runConstraint :: ∀ a g. Constraint (g a) -> g a -> Array String
runConstraint f a = unV id (const []) $ f a

allowedValues :: ∀ g a. Foldable g => (a -> String) -> Array (EqPred a) -> Constraint (g a)
allowedValues showVal as as' = for_ as' \a ->
  if matchesAny a as
  then pure unit
  else invalid ["Contains value: " <> (showVal a) <> " which is not allowed"]

notEmpty :: ∀ f a. Foldable f => Constraint (f a)
notEmpty as = if null as then invalid ["Input must contain values"] else pure unit

allowNoneOrOne :: ∀ g a. Foldable g => Array (EqPred a) -> Constraint (g a)
allowNoneOrOne as = f <<< usageCount as
  where
  f c | c > 1 = invalid ["Usage count (" <> show c <> ") for allowed elements (" <> (show as) <> ") must be 0 or 1"]
  f _ = pure unit

allowNoneOrAll :: ∀ f a. Foldable f => Array (EqPred a) -> Constraint (f a)
allowNoneOrAll as = f <<< usageCount as
  where
  f c | c /= 0 && c /= length as = invalid ["Usage count (" <> show c <> ") for allowed elements (" <> (show as) <> ") must be 0 or all"]
  f _ = pure unit

matchesAny :: ∀ a . a -> Array (EqPred a) -> Boolean
matchesAny = any <<< equals

usageCount :: ∀ f a. Foldable f => Array (EqPred a) -> f a -> Int
usageCount as as' = for_ as' incState `execState` 0
  where
  incState :: a -> State Int Unit
  incState a = get >>= \count -> if matchesAny a as then put (count + 1) else pure unit


data EqPred a = EqPred String (a -> Boolean)

instance predShow :: Show (EqPred a) where
  show (EqPred str _)= str

equals :: ∀ a . a -> EqPred a -> Boolean
equals a (EqPred _ p) = p a

reShow :: ∀ a . Eq a => (a -> String) -> a -> EqPred a
reShow f a = EqPred (f a) (_ == a)

pred :: ∀ a . Eq a => Show a => a -> EqPred a
pred = reShow show
