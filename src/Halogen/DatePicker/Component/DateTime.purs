module Halogen.Datapicker.Component.DateTime where

import Prelude
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.Datapicker.Component.Date as Date
import Halogen.Datapicker.Component.DateTime.Format as F
import Halogen.Datapicker.Component.Time as Time
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.Bifunctor (bimap)
import Data.DateTime (DateTime, date, modifyDate, modifyTime, time)
import Data.Either.Nested (Either2)
import Data.Foldable (foldMap)
import Data.Functor.Coproduct (Coproduct, coproduct, right, left)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap)
import Halogen.Datapicker.Component.Types (PickerMessage(..), PickerQuery(..), mustBeMounted)
import Partial.Unsafe (unsafePartialBecause)
-- import Halogen.Datapicker.Component.Time.Format as TimeF
-- import Halogen.Datapicker.Component.Date.Format as DateF

data DateTimeQuery a
  = HandleDateMessage Date.Message a
  | HandleTimeMessage Time.Message a

type Query = Coproduct (PickerQuery Unit DateTime) DateTimeQuery
type Message = PickerMessage DateTime

type State =
  { format :: F.Format
  , dateTime :: DateTime
  }

type ChildQuery = Coproduct2 Date.Query Time.Query
type Slot = Either2 Unit Unit
cpDate ∷ CP.ChildPath Date.Query ChildQuery Unit Slot
cpDate = CP.cp1
cpTime ∷ CP.ChildPath Time.Query ChildQuery Unit Slot
cpTime = CP.cp2


type HTML m = H.ParentHTML DateTimeQuery ChildQuery Slot m
type DSL m = H.ParentDSL State Query ChildQuery Slot Message m


picker ∷ ∀ m. F.Format -> DateTime -> H.Component HH.HTML Query Unit Message m
picker format dateTime = H.parentComponent
  { initialState: const $ {format, dateTime}
  , render: render >>> bimap (map right) right
  , eval: coproduct evalPicker evalDateTime
  , receiver: const Nothing
  }

render ∷ ∀ m. State -> HTML m
render {dateTime, format} = HH.div [HP.classes [HH.ClassName "Picker"]] $
  foldMap (pure <<< renderCommand dateTime) (unwrap format)

renderCommand :: ∀ m. DateTime -> F.Command -> HTML m
renderCommand t cmd@(F.Time fmt) = HH.slot' cpTime unit (Time.picker fmt (time t)) unit (HE.input HandleTimeMessage)
renderCommand t cmd@(F.Date fmt) = HH.slot' cpDate unit (Date.picker fmt (date t)) unit (HE.input HandleDateMessage)


evalDateTime ∷ ∀ m . DateTimeQuery ~> DSL m
evalDateTime (HandleDateMessage msg next) = do
  {dateTime} <- H.get
  let
    newDateTime = case msg of
      NotifyChange newDate -> modifyDate (const newDate) dateTime
  H.modify _{ dateTime = newDateTime }
  H.raise (NotifyChange newDateTime)
  pure next
evalDateTime (HandleTimeMessage msg next) = do
  {dateTime} <- H.get
  let
    newDateTime = case msg of
      NotifyChange newTime -> modifyTime (const newTime) dateTime
  H.modify _{ dateTime = newDateTime }
  H.raise (NotifyChange newDateTime)
  pure next

evalPicker ∷ ∀ m . (PickerQuery Unit DateTime) ~> DSL m
evalPicker (SetValue dateTime next) = do
  H.modify _{ dateTime = dateTime }
  map mustBeMounted $ H.query' cpTime unit $ H.request $ left <<< (SetValue (time dateTime))
  map mustBeMounted $ H.query' cpDate unit $ H.request $ left <<< (SetValue (date dateTime))
  pure $ next unit

evalPicker (GetValue next) = H.gets _.dateTime <#> next
