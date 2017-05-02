module Example where

import Prelude
import Data.String (replaceAll, Pattern(..), Replacement(..))

-- import Data.Map as M
import Data.List as L
import Debug.Trace as D
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Functor.Coproduct (left)
import Halogen.Datapicker.Component.Time.Format as TimeF
import Halogen.Datapicker.Component.Time as Time
import Halogen.Datapicker.Component.Types (PickerQuery(..), PickerMessage(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type TimeIdx = Int
data ExampleQuery a
  = SetTime TimeIdx String a
  | HandleTimeMessage TimeIdx Time.Message a

type State = Unit
data ExampleSlot = TimeSlot TimeIdx
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
  render ∷ State -> H.ParentHTML ExampleQuery Time.Query ExampleSlot m
  render _ = HH.div_
    [ HH.h1_ [ HH.text "Time" ]
    , renderTime 0 (L.fromFoldable [ TimeF.Hour, TimeF.Minute])
    , setBtn 0 "13:45"
    , renderTime 1 (L.fromFoldable [ TimeF.Hour, TimeF.Minute, TimeF.Second, TimeF.Millisecond])
    , setBtn 1 "13:45:49:119"
    , renderTime 2 (L.fromFoldable [ TimeF.Minute, TimeF.Second, TimeF.Millisecond])
    , setBtn 2 "45:49:119"
    , renderTime 3 (L.fromFoldable [ TimeF.Second, TimeF.Millisecond])
    , setBtn 3 "49:119"
    , renderTime 4 (L.fromFoldable [ TimeF.Millisecond, TimeF.Hour, TimeF.Minute])
    , setBtn 4 "119:12:45"
    , HH.h1_ [ HH.text "Date" ]
    ]

  renderTime ∷ TimeIdx -> TimeF.Format -> H.ParentHTML ExampleQuery Time.Query ExampleSlot m
  renderTime idx fmt = HH.slot (TimeSlot idx) (Time.picker fmt) unit (HE.input (HandleTimeMessage idx))

  setBtn ∷ TimeIdx -> String -> H.ParentHTML ExampleQuery Time.Query ExampleSlot m
  setBtn idx str = HH.button
    [ HE.onClick $ HE.input_ $ SetTime idx $ replaceAll (Pattern ":") (Replacement "") str ]
    [ HH.text $ "set: " <> str ]

  eval ∷ ExampleQuery ~> H.ParentDSL State ExampleQuery Time.Query ExampleSlot Void m
  eval (SetTime idx str next) = do
    isSet <- H.query (TimeSlot idx) $ H.action $ left <<< (SetValue $ Left str)
    -- TODO check if isSet is Just
    -- Looks like it's `Maybe Unit`
    D.traceAnyA isSet
    pure next

  eval (HandleTimeMessage idx msg next) = do
    D.traceAnyA ({idx, msg})
    pure next
