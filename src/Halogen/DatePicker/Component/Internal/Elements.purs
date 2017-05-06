module Halogen.Datapicker.Component.Internal.Elements where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Data.Int as Int
import Data.Enum (class BoundedEnum, fromEnum, upFrom)

--TODO parse `String` into `a` here and only invoke query if it's is valid
-- TODO change signature so that we dont need to change record type if record type is chaged in `numberElement`
enumNumberElement :: ∀ query a. Show a => BoundedEnum a => (∀ b. String -> b -> query b) -> {title :: String} -> a -> H.ComponentHTML query
enumNumberElement q {title} val = numberElement q {title, min: (fromEnum (bottom :: a)), max: (fromEnum (top :: a)) } (fromEnum val)

--TODO parse `String` into `Int` here and only invoke query if it's is valid
numberElement :: ∀ query. (∀ b. String -> b -> query b) -> {title :: String, min :: Int, max :: Int} -> Int -> H.ComponentHTML query
numberElement query {title, min, max} value = HH.input
  [ HP.type_ HP.InputNumber
  , HP.title title
  , HP.value (show value)
  , HP.min (Int.toNumber min)
  , HP.max (Int.toNumber max)
  , HE.onValueChange (HE.input query)
  ]

-- TODO parse `String` into `a` here and only invoke query if it's is valid
choiseElement :: forall query a. Show a => BoundedEnum a => (∀ b. String -> b -> query b) -> {title :: String} -> a -> H.ComponentHTML query
choiseElement query {title} val = HH.select
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
