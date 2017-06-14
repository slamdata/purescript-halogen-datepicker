module Halogen.Datapicker.Component.Internal.Num
  ( picker
  , NumQuery
  , Query
  , QueryIn
  , Input
  , mkInputValue
  , numberElement
  , HasNumberInputVal
  , numberHasNumberInputVal
  , intHasNumberInputVal
  , boundedEnumHasNumberInputVal )
  where

import Prelude

import CSS as CSS
import Control.Alternative (class Alternative, empty)
import Control.Monad.Except (runExcept)
import Control.MonadPlus (guard)
import DOM.Event.Event (Event)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, fromEnum, toEnum)
import Data.Foreign (readBoolean, readString, toForeign)
import Data.Foreign.Index (readProp)
import Data.Functor.Coproduct (Coproduct, coproduct, right)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Number as N
import Data.String (Pattern(..), length, stripSuffix)
import Data.Tuple (Tuple(..), fst)
import Halogen as H
import Halogen.Datapicker.Component.Internal.Range (Range(..), isInRange, rangeMax, rangeMin)
import Halogen.Datapicker.Component.Types (BasePickerQuery(..), PickerMessage(..), toAlt)
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP


type State val =
  { number:: InputValue val
  , range:: Range val
  , title:: String
  }

-- TODO maybe replace Maybe with PickerValue
type Input val = Maybe val
data NumQuery val a = Update (InputValue val) a
type QueryIn val = BasePickerQuery Unit (Input val)
type Query val = Coproduct (QueryIn val) (NumQuery val)
type Message val = PickerMessage (Input val)

type DSL val = H.ComponentDSL (State val) (Query val) (Message val)
type HTML val = H.ComponentHTML (NumQuery val)

picker ∷ ∀ val m
  . Ord val
  => HasNumberInputVal val
  -> {title :: String, range :: Range val}
  -> H.Component HH.HTML (Query val) Unit (Message val) m
picker hasNumberInputVal {title, range} = H.component
  { initialState: const {title, range, number: emptyNumberInputValue}
  , render: (render hasNumberInputVal) <#> (map right)
  , eval: coproduct (evalPicker hasNumberInputVal) evalNumber
  , receiver: const Nothing
  }

render ∷ ∀ val. Ord val => HasNumberInputVal val -> State val -> HTML val
render hasNumberInputVal s = numberElement hasNumberInputVal Update { title:s.title, range: s.range} s.number

evalNumber ∷ ∀ val m . Eq val => NumQuery val ~> DSL val m
evalNumber (Update number next) = do
  s <- H.get
  H.modify _{number = number}
  when (number /= s.number) $ H.raise (NotifyChange $ fst number)
  pure next

toMbString :: ∀ a. HasNumberInputVal a -> Maybe a -> Maybe String
toMbString hasNumberInputVal number = (Just $ maybe "" hasNumberInputVal.toValue number)

evalPicker ∷ ∀ val m . HasNumberInputVal val -> QueryIn val ~> DSL val m
evalPicker hasNumberInputVal (SetValue number next) = do
  H.modify _{number = Tuple number (toMbString hasNumberInputVal number)}
  pure $ next unit
evalPicker _ (GetValue next) = H.gets _.number <#> (fst >>> next)


type InputValue a = Tuple (Maybe a) (Maybe String)

toString :: ∀ a. InputValue a -> String
toString (Tuple _ mbStr) = maybe "" id mbStr

mkInputValue :: ∀ a. HasNumberInputVal a -> a -> InputValue a
mkInputValue hasNumberInputVal n = Tuple (Just n) (Just $ hasNumberInputVal.toValue n)

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

numberElement :: ∀ val query
  . Ord val
  => HasNumberInputVal val
  -> (∀ b. InputValue val -> b -> query b)
  -> {title :: String, range :: Range val}
  -> InputValue val
  -> H.ComponentHTML query
numberElement hasNumberInputVal query {title, range} value = HH.input $
  [ HP.type_ HP.InputNumber
  , HP.classes classes
  , HP.title title
  , HP.value valueStr
  , HE.onInput $ HE.input $
    inputValueFromEvent
    -- TODO bug
    -- we might want number and string value to comute?
    --  i.e.  if user types `-0` we will parse it as `0`
    --  or   if user types `001` we will parse it as `1`
    --  or   if user types `0.1111111111111111111111` we will parse it as `0.1111111111111111`
    --  or   if user types `1e1` we will parse it as `10`
    -- we can't use `pattern` attr as it's not supported for number input
    -- but we can either use some regex to validate if input is valid
    -- or check if  fromString and toString give same result
    >>> lmap (_ >>= hasNumberInputVal.fromString)
    >>> isInputInRange range
    >>> query
  ]
  <> (rangeMin range <#> hasNumberInputVal.toNumber >>> HP.min # toAlt)
  <> (rangeMax range <#> hasNumberInputVal.toNumber >>> HP.max # toAlt)
  <> styles
  where
  valueStr = toString value
  classes = [HH.ClassName "Picker-input"] <> (guard (isInvalid value) $> HH.ClassName "Picker-input--invalid")
  styles = case range of
    MinMax _ _ -> []
    _ | isInvalid value -> []
    _ | isEmpty value -> [HCSS.style $ CSS.width $ CSS.em 2.25]
    _ -> [HCSS.style $ CSS.width $ CSS.em (Int.toNumber (length valueStr) * 0.5 + 1.75)]


-- We need to validate if value is in range manually as for example,
-- if `min = 0`, user still can enter `-1` in chrome.
isInputInRange :: ∀ a. Ord a => Range a -> InputValue a -> InputValue a
isInputInRange range val = lmap (_ >>= boolToAltPredicate (isInRange range)) val

boolToAltPredicate :: ∀ a f. Alternative f => (a -> Boolean) -> a -> f a
boolToAltPredicate f a =  if f a then pure a else empty

inputValueFromEvent :: Event -> InputValue String
inputValueFromEvent event = let val = validValueFromEvent event
  in Tuple val val

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



type HasNumberInputVal a  =
  { fromString :: String -> Maybe a
  , toValue :: a -> String
  , toNumber :: a -> Number
  }

numberHasNumberInputVal :: HasNumberInputVal Number
numberHasNumberInputVal =
  { fromString: N.fromString
  , toValue: showNum
  , toNumber: id
  }

intHasNumberInputVal :: HasNumberInputVal Int
intHasNumberInputVal =
  { fromString: numberHasNumberInputVal.fromString >=> Int.fromNumber
  , toValue: show
  , toNumber: Int.toNumber
  }

boundedEnumHasNumberInputVal :: ∀ a. BoundedEnum a => HasNumberInputVal a
boundedEnumHasNumberInputVal =
  { fromString: intHasNumberInputVal.fromString >=> toEnum
  , toValue: fromEnum >>> intHasNumberInputVal.toValue
  , toNumber: fromEnum >>> intHasNumberInputVal.toNumber
  }
