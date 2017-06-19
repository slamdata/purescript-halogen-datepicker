module Halogen.Datapicker.Internal.Range where

import Prelude
import Data.Enum (class BoundedEnum)
import Data.Maybe (Maybe(..))

data Range a = MinMax a a | Min a | Max a

instance rangeFunctor :: Functor Range where
  map f (MinMax a b) = MinMax (f a) (f b)
  map f (Min a) = Min (f a)
  map f (Max a) = Max (f a)

minmaxRange :: ∀ a. a -> a -> Range a
minmaxRange = MinMax

minRange :: ∀ a. a -> Range a
minRange = Min

maxRange :: ∀ a. a -> Range a
maxRange = Max

rangeMin :: Range ~> Maybe
rangeMin (MinMax m _) = Just m
rangeMin (Min m) = Just m
rangeMin _ = Nothing

rangeMax :: Range ~> Maybe
rangeMax (MinMax _ m) = Just m
rangeMax (Max m) = Just m
rangeMax _ = Nothing

isInRange :: ∀ a. Ord a => Range a -> a -> Boolean
isInRange range n = case range of
  (Min min) -> min <= n
  (Max max) -> max >= n
  (MinMax min max) -> min <= n && n <= max


bottomTop :: ∀ a. BoundedEnum a => Range a
bottomTop = minmaxRange bottom top
