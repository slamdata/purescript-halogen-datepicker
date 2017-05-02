module Halogen.Datapicker.Component.Time where

import Prelude
import Debug.Trace as D

import Data.Formatter.DateTime as FDT
import Halogen.Datapicker.Component.Types (PickerQuery(..), PickerMessage(..))
import Data.Time(Time, setSecond, setMinute, setHour, setMillisecond)
import Control.Alternative(class Alternative, empty)
import Data.Date (canonicalDate)
import Data.DateTime (DateTime(..), time)
import Data.List (List)
import Data.Foldable (foldr, fold, for_)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Tuple (Tuple(..))
import Data.Enum (class BoundedEnum, toEnum)
import Data.Map (Map, fromFoldable, toUnfoldable, insert, lookup)
import Data.Functor.Coproduct (Coproduct, coproduct, right)
import Halogen.Datapicker.Component.Time.Format as F
import Text.Parsing.Parser (Parser, runParser)
import Partial.Unsafe (unsafePartialBecause)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.Int as Int

type Message = PickerMessage
type State = Map F.Command Int
type Query = Coproduct (PickerQuery Time) TimeQuery
data TimeQuery a = UpdateCommand F.Command String a
type DSL = H.ComponentDSL State Query Message
type HTML = H.ComponentHTML TimeQuery

initialStateFromFormat ∷ F.Format -> State
initialStateFromFormat fmt = fmt <#> (\k -> Tuple k 0) # fromFoldable

timeToState ∷ F.Format -> Time -> State
timeToState f t = fromFoldable $ f <#> \c -> Tuple c $ F.toGetter c t


stateToTime ∷ State -> Time
stateToTime s = foldr f bottom ((toUnfoldable s) :: List (Tuple F.Command Int))
  where
  f (Tuple F.Second val) = setSecond $ unsafeToEnum val
  f (Tuple F.Minute val) = setMinute $ unsafeToEnum val
  f (Tuple F.Hour val) = setHour $ unsafeToEnum val
  f (Tuple F.Millisecond val) = setMillisecond $ unsafeToEnum val
  unsafeToEnum :: ∀ a. BoundedEnum a => Int -> a
  unsafeToEnum = toEnum >>> (unsafePartialBecause "state mustn't contain invalid value" fromJust)

dateTimeFromTime ∷ Time -> DateTime
dateTimeFromTime = DateTime (canonicalDate bottom bottom bottom)

toAlt :: ∀ f. Alternative f => Maybe ~> f
toAlt = maybe empty pure

mapCommands ∷ ∀ a. F.Format -> State -> (F.Command -> Int -> F.CommandProp -> a) -> Array a
mapCommands fmt s f = fold $ fmt <#> \key -> (toAlt $ lookup key s) <#> \val -> f key val (F.getProps key)

picker ∷ ∀ m. F.Format -> H.Component HH.HTML Query Unit Message m
picker fmt = H.component
  { initialState: const $ initialStateFromFormat fmt
  , render: render <#> (map right)
  , eval: coproduct evalPicker evalTime
  , receiver: const Nothing
  }
  where
  unformat ∷ Parser String Time
  unformat = FDT.unformatParser (F.toDateTimeFormatter fmt) <#> time
  format ∷ Time -> String
  format = FDT.format (F.toDateTimeFormatter fmt) <<< dateTimeFromTime
  render ∷ State -> HTML
  render s = HH.li_ $ mapCommands fmt s renderItem
  renderItem ∷ F.Command -> Int -> F.CommandProp -> HTML
  renderItem key value ({ placeholder, range: (Tuple min max) }) = HH.input
    [ HP.type_ HP.InputNumber
    , HP.placeholder placeholder
    , HP.value (show value)
    , HP.min (Int.toNumber min)
    , HP.max (Int.toNumber max)
    , HE.onValueChange (HE.input (UpdateCommand key))
    ]
  evalTime ∷ TimeQuery ~> DSL m
  evalTime (UpdateCommand key val next) = do
    for_ (Int.fromString val) \val' ->
      H.modify (insert key val') *> (H.raise NotifyChange)
    pure next
  evalPicker ∷ (PickerQuery Time) ~> DSL m
  evalPicker (SetValue strOrVal next) = do
    -- TODO this nested case can be refactored
    case strOrVal of
      Left str -> case runParser str unformat of
        Left err -> H.raise $ ParserFailed err str
        Right val -> H.put (timeToState fmt val) *> (H.raise NotifyChange)
      Right val -> H.put (timeToState fmt val) *> (H.raise NotifyChange)
    pure next
  evalPicker (GetValue next) = do
    t <- H.get <#> stateToTime
    pure $ next $ Tuple (format t) t
