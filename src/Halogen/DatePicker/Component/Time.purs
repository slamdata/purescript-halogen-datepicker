module Halogen.Datapicker.Component.Time where

import Prelude

import Data.Formatter.DateTime as FDT
import Halogen.Datapicker.Component.Types (PickerQuery(..), PickerMessage(..))
import Data.Time
  ( Time
  , second
  , minute
  , hour
  , millisecond
  , setSecond
  , setMinute
  , setHour
  , setMillisecond
  )
import Data.Date (canonicalDate)
import Data.DateTime (DateTime(..), time)
import Data.List (List)
import Data.Foldable (foldr)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Data.Enum (class BoundedEnum, fromEnum, toEnum)
import Data.Map (Map, fromFoldable, toUnfoldable)
import Halogen.Datapicker.Component.Time.Format as F
import Text.Parsing.Parser (Parser, runParser)
import Partial.Unsafe (unsafePartialBecause)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = Map F.Command Int
type Query = PickerQuery Time
type Message = PickerMessage

initialStateFromFormat ∷ F.Format -> State
initialStateFromFormat fmt = fmt <#> (\k -> Tuple k 0) # fromFoldable

timeToState ∷ Time -> State
timeToState t = fromFoldable
  [ Tuple F.Second (second t # fromEnum)
  , Tuple F.Minute (minute t # fromEnum)
  , Tuple F.Hour (hour t # fromEnum)
  , Tuple F.Millisecond (millisecond t # fromEnum)
  ]

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

mapCommands ∷ ∀ a. State -> (F.Command -> Int -> F.CommandProp -> a) -> Array a
mapCommands s f = toUnfoldable s <#> \(Tuple k v) -> f k v (F.getProps k)

picker ∷ ∀ m. F.Format -> H.Component HH.HTML Query Unit Message m
picker f = H.component
  { initialState: const $ initialStateFromFormat f
  , render: render
  , eval: eval
  , receiver: const Nothing
  }
  where
  unformat ∷ Parser String Time
  unformat = FDT.unformatParser (F.toDateTimeFormatter f) <#> time
  format ∷ Time -> String
  format = FDT.format (F.toDateTimeFormatter f) <<< dateTimeFromTime
  render ∷ State -> H.ComponentHTML Query
  render s = HH.li_ $ mapCommands s renderItem
  renderItem ∷ F.Command -> Int -> F.CommandProp -> H.ComponentHTML Query
  renderItem key value ({ placeholder }) = HH.input
    [ HP.type_ HP.InputNumber
    , HP.placeholder placeholder
    , HP.value (show value)
    -- TODO add update command to TimeQuery
    -- this is the part there TimeQ and PickerQ are different
    -- , HE.onValueChange (HE.input (UpdateCommand c))
    ]
  eval ∷ Query ~> H.ComponentDSL State Query Message m
  eval (SetValue strOrVal next) = do
    -- TODO this nested case can be refactored
    case strOrVal of
      Left str -> case runParser str unformat of
        Left err -> H.raise $ ParserFailed err str
        Right val -> H.put (timeToState val) *> (H.raise NotifyChange)
      Right val -> H.put (timeToState val) *> (H.raise NotifyChange)
    pure next
  eval (GetValue next) = do
    t <- H.get <#> stateToTime
    pure $ next $ Tuple (format t) t
