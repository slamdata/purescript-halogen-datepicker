module Halogen.Datapicker.Component.Internal.Elements where

import Prelude
import Data.Int as Int
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Control.Alternative (class Alternative, empty)
import Data.Enum (class BoundedEnum, fromEnum, upFrom)
import Data.Maybe (Maybe(..), maybe)
import Data.These (These(..), theseRight, theseLeft)

--TODO parse `String` into `a` here and only invoke query if it's is valid
-- TODO change signature so that we dont need to change record type if record type is chaged in `numberElement`
enumElement :: ∀ query a. Show a => BoundedEnum a => (∀ b. String -> b -> query b) -> {title :: String} -> a -> H.ComponentHTML query
enumElement q {title} val =
  numberElement
    q
    { title
    , range: minmaxRange
        (Int.toNumber $ fromEnum (bottom :: a))
        (Int.toNumber $ fromEnum (top :: a))
    }
    (fromEnum val)
    show

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


or :: ∀ a. a -> Maybe a -> a
or a mb = maybe a id mb

toAlt :: ∀ f. Alternative f => Maybe ~> f
toAlt (Just a) = pure a
toAlt Nothing = empty

numberElement :: ∀ a query. (∀ b. String -> b -> query b) -> {title :: String, range :: Range Number} -> a -> (a -> String) -> H.ComponentHTML query
numberElement query {title, range} value show' = HH.input $
  [ HP.type_ HP.InputNumber
  , HP.title title
  , HP.value $ show' value
  , HE.onValueChange $ HE.input query
  ]
  <> (rangeMin range <#> HP.min # toAlt)
  <> (rangeMax range <#> HP.max # toAlt)

-- TODO parse `String` into `a` here and only invoke query if it's is valid
choiceElement :: ∀ query a. Show a => BoundedEnum a => (∀ b. String -> b -> query b) -> {title :: String} -> a -> H.ComponentHTML query
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


textElement :: ∀ query. {text :: String} -> H.ComponentHTML query
textElement {text} = HH.span_ [HH.text text]
