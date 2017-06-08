module Halogen.Datapicker.Component.Internal.Number where

import Prelude
import CSS as CSS
import Debug.Trace as D
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.Functor.Coproduct (Coproduct, coproduct, right)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Halogen.Datapicker.Component.Internal.Elements (NumberInputValue, Range, emptyNumberInputValue, num, numberElement, showNum)
import Halogen.Datapicker.Component.Types (PickerMessage(..), PickerQuery(..))


type State =
  { number:: NumberInputValue
  , range:: Range Number
  , title:: String
  }

-- TODO maybe replace Maybe with PickerValue
type Input = Maybe Number
data NumberQuery a = Update NumberInputValue a
type QueryIn = PickerQuery Unit Input
type Query = Coproduct QueryIn NumberQuery
type Message = PickerMessage Input

type DSL = H.ComponentDSL State Query Message
type HTML = H.ComponentHTML NumberQuery

picker ∷ ∀ m. {title :: String, range :: Range Number} -> H.Component HH.HTML Query Unit Message m
picker {title, range} = H.component
  { initialState: const {title, range, number: emptyNumberInputValue}
  , render: render <#> (map right)
  , eval: coproduct evalPicker evalNumber
  , receiver: const Nothing
  }

render ∷ State -> HTML
render s = numberElement Update { title:s.title, range: s.range} s.number

evalNumber ∷ ∀ m . NumberQuery ~> DSL m
evalNumber (Update number next) = do
  -- D.traceAnyA "what"
  H.modify _{number = number}
  -- when (nextDuration /= s.duration) $
  H.raise (NotifyChange $ num number)
  pure next

evalPicker ∷ ∀ m . QueryIn ~> DSL m
evalPicker (SetValue number next) = do
  H.modify _{number = Tuple number (map showNum number)}
  pure $ next unit
evalPicker (GetValue next) = H.gets _.number <#> (num >>> next)




-- durationToVals :: Values -> F.Format -> Maybe IsoDuration -> Values
-- durationToVals vals format isoDuration = cleanVals
--   where
--   cleanVals :: Values
--   cleanVals =  fromFoldable $ unwrap format <#> mapper (map unIsoDuration isoDuration)
--   mapper :: Maybe Duration -> F.Command -> Tuple F.Command NumberInputValue
--   mapper Nothing cmd = Tuple cmd $ maybe
--     emptyNumberInputValue
--     id
--     (lookup cmd vals)
--   mapper (Just duration) cmd = Tuple cmd $ case lookup cmd vals, F.toGetter cmd duration of
--     Just old, Just new -> if num old == Just new then old else mkNumberInputValue new
--     Just old, Nothing -> old
--     Nothing, Just new -> mkNumberInputValue new
--     Nothing, Nothing -> zeroNumberInputValue
