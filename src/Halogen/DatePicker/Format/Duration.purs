module Halogen.Datepicker.Format.Duration
  ( Format
  , Command
  , mkFormat
  , unformat
  , format
  , toSetter
  , toGetter
  , module ReExport
  ) where

import Prelude
import Data.Formatter.Interval (unformatInterval, formatInterval)
import Data.Interval as I
import Data.Interval (DurationComponent(..)) as ReExport
import Data.Foldable (class Foldable)
import Data.Map (insert, lookup)
import Data.String (joinWith)
import Data.Array (fromFoldable)
import Data.Maybe (Maybe)
import Data.Either (Either(..))
import Data.Newtype (class Newtype, over, unwrap)
import Halogen.Datepicker.Internal.Constraint as C
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)


type Command = I.DurationComponent

newtype Format = Format (Array Command)
derive instance formatNewtype ∷ Newtype Format _
derive instance formatGeneric ∷ Generic Format _
derive instance formatEq ∷ Eq Format
derive instance formatOrd ∷ Ord Format
instance formatShow ∷ Show Format where
  show = genericShow


toSetter ∷ Command -> Number -> I.Duration -> I.Duration
toSetter cmd n d = over I.Duration (insert cmd n) d

toGetter ∷ Command -> I.Duration -> Maybe Number
toGetter cmd = unwrap >>> lookup cmd

mkFormat ∷ ∀ f. Foldable f => f Command -> Either String Format
mkFormat cmds = if (errs /= [])
  then Left $ joinWith "; " errs
  else pure $ Format fmt
  where
  fmt ∷ Array Command
  fmt = fromFoldable cmds
  errs = C.runConstraint formatConstraint fmt

unformat ∷ String -> Either String I.IsoDuration
unformat str = unformatInterval str >>= case _ of
  I.JustDuration d -> pure d
  x -> Left $ "unformating of duration string returned wrong result: " <> show x

format ∷ I.IsoDuration -> String
format = formatInterval <<< I.JustDuration


formatConstraint ∷ ∀ g. Foldable g => C.Constraint (g Command)
formatConstraint = C.notEmpty <> C.sorted C.Decreasing
