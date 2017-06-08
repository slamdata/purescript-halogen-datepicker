module Halogen.Datapicker.Component.Internal.Range where

import Prelude
import Data.Maybe (Maybe)
import Data.These (These(..), theseLeft, theseRight)

type Range a = These a a

minmaxRange :: ∀ a. a -> a -> Range a
minmaxRange = Both

minRange :: ∀ a. a -> Range a
minRange = This

maxRange :: ∀ a. a -> Range a
maxRange = That

rangeMin :: Range ~> Maybe
rangeMin = theseLeft

rangeMax :: Range ~> Maybe
rangeMax = theseRight

isInRange :: ∀ a. Ord a => Range a -> a -> Boolean
isInRange range n = case range of
  (This min) -> min <= n
  (That max) -> max >= n
  (Both min max) -> min <= n && n >= max
