module Halogen.Datapicker.Component.Internal.Num
  ( picker
  , NumberQuery
  , Query
  , QueryIn
  , Input
  , mkNumberInputValue
  , numberElement )
  where

import Prelude
import CSS as CSS
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Control.Alternative (class Alternative, empty)
import Control.Monad.Except (runExcept)
import Control.MonadPlus (guard)
import DOM.Event.Event (Event)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foreign (readBoolean, readString, toForeign)
import Data.Foreign.Index (readProp)
import Data.Functor.Coproduct (Coproduct, coproduct, right)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.Number (fromString)
import Data.String (Pattern(..), length, stripSuffix)
import Data.These (These(..))
import Data.Tuple (Tuple(..), fst)
import Halogen.Datapicker.Component.Internal.Range (Range, isInRange, rangeMax, rangeMin)
import Halogen.Datapicker.Component.Types (PickerMessage(..), PickerQuery(..))


type State =
  { number:: InputValue Number
  , range:: Range Number
  , title:: String
  }

-- TODO maybe replace Maybe with PickerValue
type Input = Maybe Number
data NumberQuery a = Update (InputValue Number) a
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
  s <- H.get
  H.modify _{number = number}
  when (number /= s.number) $ H.raise (NotifyChange $ fst number)
  pure next

toMbString :: Maybe Number -> Maybe String
toMbString number = (Just $ maybe "" showNum number)

evalPicker ∷ ∀ m . QueryIn ~> DSL m
evalPicker (SetValue number next) = do
  H.modify _{number = Tuple number (toMbString number)}
  pure $ next unit
evalPicker (GetValue next) = H.gets _.number <#> (fst >>> next)


type InputValue a = Tuple (Maybe a) (Maybe String)

toString :: ∀ a. InputValue a -> String
toString (Tuple _ mbStr) = maybe "" id mbStr

mkNumberInputValue :: Number -> InputValue Number
mkNumberInputValue n = Tuple (Just n) (Just $ showNum n)

emptyNumberInputValue :: ∀ a. InputValue a
emptyNumberInputValue = Tuple Nothing (Just "")

isInvalid  :: ∀ a. InputValue a -> Boolean
isInvalid (Tuple Nothing (Just "")) = false
isInvalid (Tuple Nothing (Just _)) = true
isInvalid (Tuple _ Nothing) = true
isInvalid _ = false

isEmpty  :: ∀ a. InputValue a -> Boolean
isEmpty (Tuple _ (Just "")) = true
isEmpty _ = false

showNum :: Number -> String
showNum 0.0 = "0"
showNum n = let str = show n
  in maybe str id (stripSuffix (Pattern ".0") str)

numberElement :: ∀ query
  . (∀ b. InputValue Number -> b -> query b)
  -> {title :: String, range :: Range Number}
  -> InputValue Number
  -> H.ComponentHTML query
numberElement query {title, range} value = HH.input $
  [ HP.type_ HP.InputNumber
  , HP.classes classes
  , HP.title title
  , HP.value valueStr
  , HE.onInput $ HE.input $
    inputValueFromEvent
    >>> lmap (_ >>= fromString)
    >>> isNumberInputInRange range
    >>> query
  ]
  <> (rangeMin range <#> HP.min # toAlt)
  <> (rangeMax range <#> HP.max # toAlt)
  <> styles
  where
  valueStr = toString value
  classes = [HH.ClassName "Picker-input"]
    <> (guard (isInvalid value) $> HH.ClassName "Picker-input--invalid")
  -- If there is no `min` or `max` props then `input` will not have proper size, so we set width to
  -- `3 + {number of characters in value})ch`, so that it's not taking too much space
  -- (the 3 is sum of: 2 for increment/decrement buttons and 1 for extra free space).
  styles = case range of
    Both _ _ -> []
    _ | isInvalid value || isEmpty value -> []
    _ -> [HCSS.style $ CSS.width (CSS.Size (CSS.value (toNumber $ length valueStr + 3) <> CSS.fromString "ch"))]


-- We need to validate if value is in range manually as for example,
-- if `min = 0`, user still can enter `-1` in chrome.
isNumberInputInRange :: Range Number -> InputValue Number -> InputValue Number
isNumberInputInRange range val = lmap (_ >>= boolToAltPredicate (isInRange range)) val

boolToAltPredicate :: ∀ a f. Alternative f => (a -> Boolean) -> a -> f a
boolToAltPredicate f a = if f a then pure a else empty

inputValueFromEvent :: Event -> InputValue String
inputValueFromEvent event = Tuple str str
  where
  str = validValueFromEvent event

validValueFromEvent :: Event -> Maybe String
validValueFromEvent event = unF $ do
  target <- readProp "target" $ toForeign event
  validity <- readProp "validity" target
  badInput <- readProp "badInput" validity >>= readBoolean
  value <- readProp "value" target >>= readString
  pure (if badInput then Nothing else Just value)
  where
  unF e = case runExcept e of
    Left _ -> Nothing
    Right x -> x

toAlt :: ∀ f. Alternative f => Maybe ~> f
toAlt (Just a) = pure a
toAlt Nothing = empty
