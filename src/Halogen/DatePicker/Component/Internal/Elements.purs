module Halogen.Datapicker.Component.Internal.Elements where

import Prelude
import CSS as CSS
import Data.Int as Int
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Control.Alternative (class Alternative, empty)
import Control.MonadPlus (guard)
import Data.Enum (class BoundedEnum, fromEnum, upFrom)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.Number (fromString)
import Data.String (Pattern(..), length, stripSuffix)
import Data.These (These(..), theseRight, theseLeft)
import Data.Tuple (Tuple(..), fst, snd)

--TODO parse `String` into `a` here and only invoke query if it's is valid
-- TODO change signature so that we dont need to change record type if record type is chaged in `numberElement`
enumElement :: ∀ query a
  . Show a
  => BoundedEnum a
  => (∀ b. String -> b -> query b)
  -> {title :: String}
  -> a
  -> H.ComponentHTML query
enumElement q {title} val =
  numberElement
    (snd >>> q)
    { title
    , range: minmaxRange
        (Int.toNumber $ fromEnum (bottom :: a))
        (Int.toNumber $ fromEnum (top :: a))
    }
    (mkNumberInputValue $ toNumber $ fromEnum val)


type Range a = These a a

minmaxRange :: ∀ a. a -> a -> Range a
minmaxRange = Both

minRange :: ∀ a. a -> Range a
minRange = This

maxRange :: ∀ a. a -> Range a
maxRange = That

rangeMin :: Range ~> Maybe
rangeMin = theseLeft

rangeMax :: Range ~> Maybe
rangeMax = theseRight


toAlt :: ∀ f. Alternative f => Maybe ~> f
toAlt (Just a) = pure a
toAlt Nothing = empty

type NumberInputValue = Tuple (Maybe Number) String

num :: NumberInputValue -> Maybe Number
num = fst

mkNumberInputValue :: Number -> NumberInputValue
mkNumberInputValue n = Tuple (Just n) (showNum n)

emptyNumberInputValue :: NumberInputValue
emptyNumberInputValue = Tuple Nothing ""

zeroNumberInputValue :: NumberInputValue
zeroNumberInputValue = Tuple (Just 0.0) "0"

showNum :: Number -> String
showNum 0.0 = "0"
showNum n = let str = show n
  in maybe str id (stripSuffix (Pattern ".0") str)


numberElement :: ∀ query
  . (∀ b. NumberInputValue -> b -> query b)
  -> {title :: String, range :: Range Number}
  -> NumberInputValue
  -> H.ComponentHTML query
numberElement query {title, range} (Tuple _ value) = HH.input $
  [ HP.type_ HP.InputNumber
  , HP.classes ([HH.ClassName "Picker-input"] <> (guard (value == "") $> HH.ClassName "Picker-input--invalid"))
  , HP.title title
  , HP.value value
  , HE.onValueInput $ HE.input \str -> query (Tuple (fromString str) str)
  ]
  <> (rangeMin range <#> HP.min # toAlt)
  <> (rangeMax range <#> HP.max # toAlt)
  <> styles
  where
  -- if there is no min or max prop then element will not have proper sizing so we set
  -- `(3 + {number of characters in value})ch` as it's width so it's not taking too much space
  -- (2 for increment/decrement buttons and 1 for extra free space)
  styles = case range of
    Both _ _ -> []
    _ | value /= "" -> [HCSS.style $ CSS.width (CSS.Size (CSS.value (toNumber $ length value + 3) <> CSS.fromString "ch"))]
    _ -> []

-- TODO parse `String` into `a` here and only invoke query if it's is valid
choiceElement :: ∀ query a
  . Show a
  => BoundedEnum a
  => (∀ b. String -> b -> query b)
  -> {title :: String}
  -> a
  -> H.ComponentHTML query
choiceElement query {title} val = HH.select
  [ HE.onValueChange (HE.input query)
  , HP.title title
  ] (upFrom bottom <#> renderVal)
  where
  renderVal val' = HH.option
    [ HP.value $ show $ fromEnum val'
    , HP.selected (val' == val)
    ]
    [ HH.text $ show val' ]

textElement :: ∀ p i. {text :: String} -> H.HTML p i
textElement {text} = HH.span [HP.classes [HH.ClassName "Picker-placeholder"]] [HH.text text]
