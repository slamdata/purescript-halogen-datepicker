module Halogen.Datepicker.Internal.Elements where

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP


textElement ∷ ∀ p i. {text ∷ String} -> H.HTML p i
textElement {text} = HH.span [HP.classes [HH.ClassName "Picker-placeholder"]] [HH.text text]
