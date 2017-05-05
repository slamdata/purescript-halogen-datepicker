module Halogen.Datapicker.Component.Elements where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Data.Int as Int

numberElement :: ∀ query. (∀ b. String -> b -> query b) -> {title :: String, min :: Int, max :: Int} -> Int -> H.ComponentHTML query
numberElement query {title, min, max} value = HH.input
  [ HP.type_ HP.InputNumber
  , HP.title title
  , HP.value (show value)
  , HP.min (Int.toNumber min)
  , HP.max (Int.toNumber max)
  , HE.onValueChange (HE.input query)
  ]
