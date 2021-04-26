module Halogen.Datepicker.Internal.Num where

import Prelude

import CSS as CSS
import Control.Alternative (class Alternative, empty)
import Control.Monad.Except (runExcept)
import Control.MonadPlus (guard)
import Data.Bifunctor (lmap)
import Data.Enum (class BoundedEnum, fromEnum, toEnum)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number as N
import Data.String (Pattern(..), length, stripSuffix)
import Data.Tuple (Tuple(..), fst)
import Foreign (readBoolean, readString, unsafeToForeign)
import Foreign.Index (readProp)
import Halogen as H
import Halogen.Datepicker.Component.Types (BasePickerQuery(..))
import Halogen.Datepicker.Internal.Range (Range(..), isInRange, rangeMax, rangeMin)
import Halogen.Datepicker.Internal.Utils (asRight)
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Core (ClassName)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (Event)

type State val = InputValue val

type Message val = Maybe val
type Input val = Maybe val

type Query val = BasePickerQuery Unit (Input val)
type InputValue val = Tuple (Maybe val) (Maybe String)

type Slots ∷ ∀ k. Row k
type Slots = ()

type Slot val = H.Slot (Query val) (Message val)

type DSL val = H.HalogenM (State val) (InputValue val) Slots (Message val)
type HTML val m = H.ComponentHTML (InputValue val) Slots m

type Config val =
  { title ∷ String
  , placeholder ∷ String
  , range ∷ Range val
  , root ∷ Array ClassName
  , rootInvalid ∷ Array ClassName
  , rootLength ∷ Int → Array ClassName
  }

picker
  ∷ ∀ val m
  . Ord val
  ⇒ HasNumberInputVal val
  → Config val
  → H.Component (Query val) Unit (Message val) m
picker hasNumberInputVal conf =
  H.mkComponent
    { initialState: const emptyNumberInputValue
    , render: render hasNumberInputVal conf
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery hasNumberInputVal
        }
    }

render
  ∷ ∀ val m
  . Ord val
  ⇒ HasNumberInputVal val
  → Config val
  → State val
  → HTML val m
render hasNumberInputVal conf num = numberElement hasNumberInputVal conf num

handleAction ∷ ∀ val m. Eq val ⇒ InputValue val → DSL val m Unit
handleAction number = do
  prevNumber ← H.get
  H.put number
  unless (number == prevNumber) $ H.raise (fst number)

toMbString
  ∷ ∀ a
  . HasNumberInputVal a
  → Maybe a
  → Maybe String
toMbString hasNumberInputVal number = (Just $ maybe "" hasNumberInputVal.toValue number)

handleQuery ∷ ∀ val m a. HasNumberInputVal val → Query val a → DSL val m (Maybe a)
handleQuery hasNumberInputVal = case _ of
  SetValue number k → do
    H.put $ Tuple number (toMbString hasNumberInputVal number)
    pure $ Just (k unit)
  GetValue k →
    Just <<< k <<< fst <$> H.get

toString ∷ ∀ a. InputValue a → String
toString (Tuple _ mbStr) = fromMaybe "" mbStr

mkInputValue ∷ ∀ a. HasNumberInputVal a → a → InputValue a
mkInputValue hasNumberInputVal n =
  Tuple (Just n) (Just $ hasNumberInputVal.toValue n)

emptyNumberInputValue ∷ ∀ a. InputValue a
emptyNumberInputValue = Tuple Nothing (Just "")

isInvalid ∷ ∀ a. InputValue a → Boolean
isInvalid (Tuple Nothing (Just "")) = false
isInvalid (Tuple Nothing (Just _)) = true
isInvalid (Tuple _ Nothing) = true
isInvalid _ = false

isEmpty ∷ ∀ a. InputValue a → Boolean
isEmpty (Tuple _ (Just "")) = true
isEmpty _ = false

showNum ∷ Number → String
showNum 0.0 = "0"
showNum n = let str = show n
  in fromMaybe str (stripSuffix (Pattern ".0") str)

numberElement
  ∷ ∀ val m
  . Ord val
  ⇒ HasNumberInputVal val
  → Config val
  → InputValue val
  → HTML val m
numberElement hasNumberInputVal conf value = HH.input $
  [ HP.type_ HP.InputNumber
  , HP.classes classes
  , HP.title conf.title
  , HP.placeholder conf.placeholder
  , HP.value valueStr
  , HE.onInput \val → isInputInRange conf.range (parseValidInput (inputValueFromEvent val))
  ]
  <> (toArray (rangeMin conf.range) <#> hasNumberInputVal.toNumber >>> HP.min)
  <> (toArray (rangeMax conf.range) <#> hasNumberInputVal.toNumber >>> HP.max)
  <> [styles]
  where
  toArray = maybe [] pure
  -- Number and String value must comute (`map toValue (fromString x) == Just x`)
  -- to avoid this issues:
  --  * if user types `-0` we will parse it as `0` or
  --  * if user types `001` we will parse it as `1` or
  --  * if user types `0.1111111111111111111111` we will parse it as `0.1111111111111111` or
  --  * if user types `1e1` we will parse it as `10`
  parseValidInput ∷ InputValue String → InputValue val
  parseValidInput = lmap $ (=<<) \str → do
    val ← hasNumberInputVal.fromString str
    guard (hasNumberInputVal.toValue val == str)
    pure val

  valueStr = toString value
  sizeClass = case conf.range of
    MinMax minVal maxVal →
      conf.rootLength (max
        (length $ hasNumberInputVal.toValue minVal)
        (length $ hasNumberInputVal.toValue maxVal)
      )
    _ → []
  classes = conf.root
    <> sizeClass
    <> (guard (isInvalid value) *> conf.rootInvalid)
  controlWidth = 0.75
  styles = HCSS.style do
    case conf.range of
      MinMax _ _ → pure unit
      _ | isInvalid value → pure unit
      _ | isEmpty value → CSS.width $ CSS.em 2.25
      _ → CSS.width $ CSS.em (Int.toNumber (length valueStr) * 0.5 + 1.0 + controlWidth)


-- We need to validate if value is in range manually as for example,
-- if `min = 0`, user still can enter `-1` in chrome.
isInputInRange ∷ ∀ a. Ord a ⇒ Range a → InputValue a → InputValue a
isInputInRange range val = lmap (_ >>= boolToAltPredicate (isInRange range)) val

boolToAltPredicate ∷ ∀ a f. Alternative f ⇒ (a → Boolean) → a → f a
boolToAltPredicate f a =  if f a then pure a else empty

inputValueFromEvent ∷ Event → InputValue String
inputValueFromEvent event = let val = validValueFromEvent event
  in Tuple val val

validValueFromEvent ∷ Event → Maybe String
validValueFromEvent event = join $ asRight $ runExcept $ do
  target ← readProp "target" $ unsafeToForeign event
  validity ← readProp "validity" target
  badInput ← readProp "badInput" validity >>= readBoolean
  value ← readProp "value" target >>= readString
  pure (if badInput then Nothing else Just value)

type HasNumberInputVal a =
  { fromString ∷ String → Maybe a
  , toValue ∷ a → String
  , toNumber ∷ a → Number
  }

numberHasNumberInputVal ∷ HasNumberInputVal Number
numberHasNumberInputVal =
  { fromString: N.fromString
  , toValue: showNum
  , toNumber: identity
  }

intHasNumberInputVal ∷ HasNumberInputVal Int
intHasNumberInputVal =
  { fromString: numberHasNumberInputVal.fromString >=> Int.fromNumber
  , toValue: show
  , toNumber: Int.toNumber
  }

boundedEnumHasNumberInputVal ∷ ∀ a. BoundedEnum a ⇒ HasNumberInputVal a
boundedEnumHasNumberInputVal =
  { fromString: intHasNumberInputVal.fromString >=> toEnum
  , toValue: fromEnum >>> intHasNumberInputVal.toValue
  , toNumber: fromEnum >>> intHasNumberInputVal.toNumber
  }
