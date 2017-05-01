module Halogen.Datapicker.Component.Time where

import Prelude

import Data.Formatter.DateTime as FDT
import Halogen.Datapicker.Component.Types (PickerQuery(..), PickerMessage(..), Picker)
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

type UIState = Map F.Command Int

initialStateFromFormat ∷ F.Format -> UIState
initialStateFromFormat fmt = fmt <#> (\k -> Tuple k 0) # fromFoldable

timeToState ∷ Time -> UIState
timeToState t = fromFoldable
  [ Tuple F.Second (second t # fromEnum)
  , Tuple F.Minute (minute t # fromEnum)
  , Tuple F.Hour (hour t # fromEnum)
  , Tuple F.Millisecond (millisecond t # fromEnum)
  ]

stateToTime ∷ UIState -> Time
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

mapCommands ∷ ∀ a. UIState -> (F.Command -> Int -> F.CommandProp -> a) -> Array a
mapCommands s f = toUnfoldable s <#> \(Tuple k v) -> f k v (F.getProps k)

picker ∷ ∀ m. F.Format -> H.Component HH.HTML (PickerQuery Time) Unit PickerMessage m
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
  render ∷ UIState -> H.ComponentHTML (PickerQuery Time)
  render s = HH.li_ $ mapCommands s renderItem
  renderItem ∷ F.Command -> Int -> F.CommandProp -> H.ComponentHTML (PickerQuery Time)
  renderItem key value ({ placeholder }) = HH.input
    [ HP.type_ HP.InputNumber
    , HP.placeholder placeholder
    , HP.value (show value)
    -- TODO add update command to TimeQuery
    -- this is the part there TimeQ and PickerQ are different
    -- , HE.onValueChange (HE.input (UpdateCommand c))
    ]
  eval ∷ (PickerQuery Time) ~> H.ComponentDSL UIState (PickerQuery Time) PickerMessage m
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
