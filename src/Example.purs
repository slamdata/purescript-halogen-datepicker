module Example where

import Prelude

-- import Data.Map as M
import Data.List as L
import Data.Maybe (Maybe(..))

import Data.Time (Time)

import Halogen.Datapicker.Component.Time.Format(Command(..))
import Halogen.Datapicker.Component.Time (picker)
import Halogen.Datapicker.Component.Types (PickerQuery(..), PickerMessage(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

data ExampleQuery a
  = SetTime String a
  | HandleTimeMessage PickerMessage a

type State = Unit
data ExampleSlot = TimeSlot
derive instance eqExampleSlot ∷ Eq ExampleSlot
derive instance ordExampleSlot ∷ Ord ExampleSlot

main ∷ forall m. Applicative m => H.Component HH.HTML ExampleQuery Unit Void m
main =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
  initialState = unit
  render ∷ State -> H.ParentHTML ExampleQuery (PickerQuery Time) ExampleSlot m
  render _ = HH.div_
    [ HH.h1_ [ HH.text "example" ]
    , renderTime
    , HH.button
      [ HE.onClick (HE.input_ $ SetTime "13:45:50:599") ]
      [ HH.text "set 13:45:50:599" ]
    ]

  renderTime ∷ H.ParentHTML ExampleQuery (PickerQuery Time) ExampleSlot m
  renderTime = HH.slot TimeSlot (picker timeFormat) unit (HE.input (HandleTimeMessage))

  timeFormat = L.fromFoldable [Hour, Minute, Second, Millisecond]

  eval ∷ ExampleQuery ~> H.ParentDSL State ExampleQuery (PickerQuery Time) ExampleSlot Void m
  eval (SetTime str next) = do
    -- toggled <- H.query $ H.action $ SetValue $ Left str
    -- TODO check if toggled is Just
    pure next

  eval (HandleTimeMessage msg next) = do
    pure next
