module Halogen.Datapicker.Component.DateTime where

import Prelude
import Debug.Trace as D

import Halogen.Datapicker.Component.Internal.Elements (textElement)
import Halogen.Datapicker.Component.Types (PickerQuery(..), PickerMessage(..))
import Data.DateTime (DateTime(..), date, time, modifyDate, modifyTime )
import Data.Date (canonicalDate)
import Data.Newtype (unwrap)
import Data.Bifunctor (bimap)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), fromJust)
import Data.Enum (toEnum)
import Data.Functor.Coproduct (Coproduct, coproduct, right, left)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Either.Nested (Either2)
import Partial.Unsafe (unsafePartialBecause)
import Halogen.Component.ChildPath as CP
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

import Halogen.Datapicker.Component.DateTime.Format as F
import Halogen.Datapicker.Component.Time as Time
import Halogen.Datapicker.Component.Date as Date
-- import Halogen.Datapicker.Component.Time.Format as TimeF
-- import Halogen.Datapicker.Component.Date.Format as DateF

data DateTimeQuery a
  = HandleDateMessage Date.Message a
  | HandleTimeMessage Time.Message a

type Input = DateTime
type Query = Coproduct (PickerQuery DateTime) DateTimeQuery
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


initialStateFromFormat ∷ F.Format -> State
initialStateFromFormat format = {format: format, dateTime: DateTime (canonicalDate year bottom bottom) bottom}
  where year = unsafePartialBecause "unreachable as `0` year is in bounds" fromJust $ toEnum 0


-- picker ∷ ∀ m. F.Format -> H.Component HH.HTML Query Input Message m
picker ∷ ∀ m. F.Format -> H.Component HH.HTML Query Unit  Message m
picker fmt = H.parentComponent
  { initialState: const $ initialStateFromFormat fmt
  , render: render >>> bimap (map right) right
  , eval: coproduct evalPicker evalDateTime
  -- , receiver: \a -> Just $ H.action $ left <<< (SetValue a)
  , receiver: const Nothing
  }
  where
  render ∷ State -> HTML m
  render {dateTime, format} = HH.ul_ $ foldMap (pure <<< f) (unwrap format)
    where
    f cmd = HH.li_ [renderCommand dateTime cmd]

renderCommand :: ∀ m. DateTime -> F.Command -> HTML m
renderCommand t cmd@(F.Time fmt) = HH.slot' cpTime unit (Time.picker fmt) unit (HE.input HandleTimeMessage)
-- renderCommand t cmd@(F.Time fmt) = HH.slot' cpTime unit (Time.picker fmt) (time t) (HE.input HandleTimeMessage)
renderCommand t cmd@(F.Date fmt) = HH.slot' cpDate unit (Date.picker fmt) unit (HE.input HandleDateMessage)
-- renderCommand t cmd@(F.Date fmt) = HH.slot' cpDate unit (Date.picker fmt) (date t) (HE.input HandleDateMessage)
-- renderCommand _ (F.Placeholder str)  = textElement { text: str}


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

evalPicker ∷ ∀ m . (PickerQuery DateTime) ~> DSL m
evalPicker (SetValue dateTime next) = do
  H.modify _{ dateTime = dateTime }
  void $ H.query' cpTime unit $ H.action $ left <<< (SetValue (time dateTime))
  void $ H.query' cpDate unit $ H.action $ left <<< (SetValue (date dateTime))
  -- TODO this pattern will cause loop when parent changes value on once childe
  -- reaisis NotifyChange we should not raise this on SetValue or add a flag
  --  indicating that it was changed from ui or from parent
  -- H.raise (NotifyChange time)
  pure next
evalPicker (GetValue next) = do
  H.gets _.dateTime <#> next
