module Halogen.Datapicker.Component.Duration.Format
  ( Format
  , Command(..)
  , mkFormat
  , unformat
  , format
  , toSetter
  , toGetter
  ) where

import Prelude
import Data.Formatter.Interval (unformatInterval, formatInterval)
import Data.Interval as I
import Data.Foldable (class Foldable)
import Data.Map (insert, lookup)
import Data.String (joinWith)
import Data.List (List, fromFoldable)
import Data.Maybe (Maybe)
import Data.Either (Either(..))
import Data.Newtype (class Newtype, over, unwrap)
import Halogen.Datapicker.Component.Internal.Constraint as C
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)


data Command = Second | Minute | Hour | Day | Month | Year
derive instance commandGeneric :: Generic Command _
derive instance commandEq :: Eq Command
derive instance commandOrd :: Ord Command
instance commandShow :: Show Command where
  show = genericShow

newtype Format = Format (List Command)
derive instance formatNewtype :: Newtype Format _
derive instance formatGeneric :: Generic Format _
derive instance formatEq :: Eq Format
derive instance formatOrd :: Ord Format
instance formatShow :: Show Format where
  show = genericShow


toSetter :: Command -> Number -> I.Duration -> I.Duration
toSetter Second n d      = over I.Duration (insert I.Second n) d
toSetter Minute n d      = over I.Duration (insert I.Minute n) d
toSetter Hour n d        = over I.Duration (insert I.Hour n) d
toSetter Day n d         = over I.Duration (insert I.Day n) d
toSetter Month n d       = over I.Duration (insert I.Month n) d
toSetter Year n d        = over I.Duration (insert I.Year n) d

toGetter :: Command -> I.Duration -> Maybe Number
toGetter Second      = unwrap >>> lookup I.Second
toGetter Minute      = unwrap >>> lookup I.Minute
toGetter Hour        = unwrap >>> lookup I.Hour
toGetter Day         = unwrap >>> lookup I.Day
toGetter Month       = unwrap >>> lookup I.Month
toGetter Year        = unwrap >>> lookup I.Year

mkFormat :: ∀ f. Foldable f => f Command -> Either String Format
mkFormat cmds = if (errs /= [])
  then Left $ joinWith "; " errs
  else pure $ Format fmt
  where
  fmt :: List Command
  fmt = (fromFoldable cmds)
  errs = C.runConstraint formatConstraint fmt

unformat ∷ String -> Either String I.IsoDuration
unformat str = unformatInterval str >>= case _ of
  I.JustDuration d -> pure d
  x -> Left $ "unformating of duration string returned wrong result: " <> show x

format ∷ I.IsoDuration -> String
format = formatInterval <<< I.JustDuration


formatConstraint :: ∀ g. Foldable g => C.Constraint (g Command)
formatConstraint = C.notEmpty <> C.sorted C.Increasing
