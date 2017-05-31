module Halogen.Datapicker.Component.DateTime.Format
  ( Format
  , Command(..)
  , fromString
  , toDateTimeFormatter
  , unformat
  , format
  ) where

import Prelude
import Data.Foldable (foldMap)
import Data.String (joinWith)
import Control.Alt ((<|>))
import Data.List (List(..), null, span)
import Data.Tuple (Tuple(..))
import Data.Bifunctor (bimap)
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (class Newtype)
import Data.Formatter.DateTime as FDT
import Control.Monad.State.Trans (StateT, put, get, evalStateT, lift)
import Halogen.Datapicker.Component.Internal.Constraint as C
import Halogen.Datapicker.Component.Time.Format as TimeF
import Halogen.Datapicker.Component.Date.Format as DateF
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data Command
  = Date DateF.Format
  | Time TimeF.Format

  -- placeholder could be as part of Date or Time for now
  -- we should take a look at first and last commands of date and time and if
  -- thay contain Placeholder extract it into DateTime format level whis way
  -- we can merge last placeholder of first element with the first of the second
  -- | Placeholder String

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
-- derive instance formatSemigroup :: Semigroup Format
instance formatShow :: Show Format where
  show = genericShow

fromString :: String -> Either String Format
fromString s = FDT.parseFormatString s >>= fromDateTimeFormatter

fromDateTimeFormatter :: FDT.Formatter -> Either String Format
fromDateTimeFormatter fmt = run $ go Nil
  where
  run :: StateT FDT.Formatter (Either String) (List Command) -> Either String Format
  run s = do
    resFmt <- evalStateT s fmt
    let errs = C.runConstraint formatConstraint resFmt
    when (errs /= []) $ Left $ joinWith "; " errs
    pure $ Format resFmt

go :: List Command -> StateT FDT.Formatter (Either String) (List Command)
go currFmt = get >>= \a -> case (takeDate a <|> takeTime a) of
  Just (Left err)  -> lift $ Left err
  Just (Right (Tuple restRes restFmt)) -> let res = put restFmt *> pure (currFmt <> restRes) in
    if null restFmt then res else res >>= go
  Nothing -> get >>= \restFmt -> lift $ Left $ "left unconsumed format: " <> show restFmt

takeDate :: FDT.Formatter -> Maybe (Either String (Tuple (List Command) FDT.Formatter))
takeDate = consumeWhile
  (DateF.toCommand >>> isJust)
  (DateF.fromDateTimeFormatter >>> bimap
    ("Date Format error: " <> _ )
    (Date >>> pure))

takeTime :: FDT.Formatter -> Maybe (Either String (Tuple (List Command) FDT.Formatter))
takeTime = consumeWhile
  (TimeF.toCommand >>> isJust)
  (TimeF.fromDateTimeFormatter >>> bimap
    ("Time Format error: " <> _ )
    (Time >>> pure))

consumeWhile :: ∀ a
  . (FDT.FormatterCommand → Boolean)
  → (List FDT.FormatterCommand → Either String a)
  → List FDT.FormatterCommand
  → Maybe (Either String (Tuple a FDT.Formatter))
consumeWhile whileFn consumer fmt = span whileFn fmt #
  \({init, rest}) -> if null init then Nothing
    else Just $ consumer init <#> (\a -> Tuple a rest)

toDateTimeFormatter ∷ Format -> FDT.Formatter
toDateTimeFormatter (Format fmt) = foldMap toDTCommand fmt
  where
  toDTCommand (Date inFmt) = DateF.toDateTimeFormatter inFmt
  toDTCommand (Time inFmt) = TimeF.toDateTimeFormatter inFmt
  -- toDTCommand (Placeholder str) = FDT.Placeholder str

unformat ∷ Format -> String -> Either String DateTime
unformat fmt str = FDT.unformat (toDateTimeFormatter fmt) str

format ∷ Format -> DateTime -> String
format fmt = FDT.format (toDateTimeFormatter fmt)


formatConstraint :: C.Constraint (List Command)
formatConstraint
  =  C.notEmpty
  <> C.allowNoneOrOne [ C.EqPred "Date" isDate ]
  <> C.allowNoneOrOne [ C.EqPred "Time" isTime ]
  where
  isDate :: Command -> Boolean
  isDate (Date _) = true
  isDate _ = false

  isTime :: Command -> Boolean
  isTime (Time _) = true
  isTime _ = false