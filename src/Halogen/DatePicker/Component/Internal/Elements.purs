module Halogen.Datapicker.Component.Internal.Elements where

import Prelude
import Halogen.Datapicker.Component.Internal.Range (minmaxRange)
import Data.Int as Int
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.Enum (class BoundedEnum, fromEnum, upFrom)
import Data.Int (toNumber)
import Data.Maybe (maybe)
import Data.Tuple (snd)
import Halogen.Datapicker.Component.Internal.Number (mkNumberInputValue, numberElement)

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
    (snd >>> maybe "" id >>> q) -- TODO make sure this is what we want here or update enumElements
    { title
    , range: minmaxRange
        (Int.toNumber $ fromEnum (bottom :: a))
        (Int.toNumber $ fromEnum (top :: a))
    }
    (mkNumberInputValue $ toNumber $ fromEnum val)



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
