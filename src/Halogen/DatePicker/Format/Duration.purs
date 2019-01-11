module Halogen.Datepicker.Format.Duration
  ( module Halogen.Datepicker.Format.Duration
  , module ReExport
  ) where

import Prelude

import Data.Array (fromFoldable, null)
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.Formatter.Interval (unformatInterval, formatInterval)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Interval (DurationComponent(..)) as ReExport
import Data.Interval.Duration.Iso (IsoDuration)
import Data.Interval as I
import Data.Map (insert, lookup)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, over, unwrap)
import Data.String (joinWith)
import Halogen.Datepicker.Internal.Constraint as C


type Command = I.DurationComponent

newtype Format = Format (Array Command)
derive instance formatNewtype ∷ Newtype Format _
derive instance formatGeneric ∷ Generic Format _
derive instance formatEq ∷ Eq Format
derive instance formatOrd ∷ Ord Format
instance formatShow ∷ Show Format where
  show = genericShow


toSetter ∷ Command → Number → I.Duration → I.Duration
toSetter cmd n d = over I.Duration (insert cmd n) d

toGetter ∷ Command → I.Duration → Maybe Number
toGetter cmd = unwrap >>> lookup cmd

mkFormat ∷ ∀ f. Foldable f ⇒ f Command → Either String Format
mkFormat cmds = if (not $ null errs)
  then Left $ joinWith "; " errs
  else pure $ Format fmt
  where
  fmt ∷ Array Command
  fmt = fromFoldable cmds
  errs = C.runConstraint formatConstraint fmt

unformat ∷ String → Either String IsoDuration
unformat str = unformatInterval str >>= case _ of
  I.DurationOnly d → pure d
  x → Left $ "unformating of duration string returned wrong result: " <> show x

format ∷ IsoDuration → String
format = formatInterval <<< I.DurationOnly


formatConstraint ∷ ∀ g. Foldable g ⇒ C.Constraint (g Command)
formatConstraint = C.notEmpty <> C.sorted C.Decreasing
