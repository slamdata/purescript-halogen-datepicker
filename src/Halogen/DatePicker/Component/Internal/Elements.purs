module Halogen.Datapicker.Component.Internal.Elements where

import Prelude
import Halogen as H
import Halogen.Datapicker.Component.Internal.Num as N
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.Enum (class BoundedEnum, fromEnum, upFrom)
import Data.Maybe (Maybe)
import Halogen.Datapicker.Component.Internal.Range (Range)
import Halogen.Datapicker.Component.Types (PickerMessage(..))
import Halogen.Query (Action)

-- TODO change signature so that we dont need to change record type if record type is chaged in `numberElement`
enumElement :: ∀ query x slot m
  . slot
  -> (x -> Action query)
  -> {title :: String, range :: Range Int}
  -> (Maybe Int -> x)
  -> H.ParentHTML query (N.Query Int) slot m
enumElement slot q conf mkVal =
  HH.slot
    slot
    (N.picker N.intHasNumberInputVal conf)
    unit
    (HE.input $ \(NotifyChange n) -> q $ mkVal n)



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
