module Halogen.Datapicker.Component.Date where

import Prelude
import Debug.Trace as D

import Data.Enum (toEnum)
import Halogen.Datapicker.Component.Internal.Enums
  ( setYear4
  , setYear2
  , setYear
  , setMonth
  , setDay
  , monthShort
  , year4
  , year2
  )
import Halogen.Datapicker.Component.Internal.Elements (textElement, enumElement, choiceElement)
import Halogen.Datapicker.Component.Types (PickerQuery(..), PickerMessage(..))
import Data.Date
  ( Date
  , year, month, day
  , canonicalDate
  )
import Data.Newtype (unwrap)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), fromJust)
import Data.Functor.Coproduct (Coproduct, coproduct, right)
import Halogen.Datapicker.Component.Date.Format as F
import Halogen as H
import Halogen.HTML as HH
import Data.Int as Int
import Partial.Unsafe (unsafePartialBecause)

data DateQuery a = UpdateCommand F.Command String a

type Query = Coproduct (PickerQuery Date) DateQuery
type Message = PickerMessage Date
type Input = Date
type State =
  { format :: F.Format
  , date :: Date
  }

type DSL = H.ComponentDSL State Query Message
type HTML = H.ComponentHTML DateQuery

initialStateFromFormat ∷ F.Format -> State
initialStateFromFormat format = {format: format, date: canonicalDate year bottom bottom}
  where year = unsafePartialBecause "unreachable as `0` year is in bounds" fromJust $ toEnum 0

-- picker ∷ ∀ m. F.Format -> H.Component HH.HTML Query Input Message m
picker ∷ ∀ m. F.Format -> H.Component HH.HTML Query Unit  Message m
picker fmt = H.component
  { initialState: const $ initialStateFromFormat fmt
  , render: render <#> (map right)
  , eval: coproduct evalPicker evalDate
  -- , receiver: \a -> Just $ H.action $ left <<< (SetValue a)
  , receiver: const Nothing
  }
  where
  render ∷ State -> HTML
  render {date, format} = HH.ul_ $ foldMap (pure <<< f) (unwrap format)
    where
    f cmd = HH.li_ [renderCommand date cmd]

renderCommand :: Date -> F.Command -> HTML
renderCommand t cmd@F.YearFull            = enumElement (UpdateCommand cmd) { title: "Year" } (year4 t)
renderCommand t cmd@F.YearTwoDigits       = enumElement (UpdateCommand cmd) { title: "Year" } (year2 t)
renderCommand t cmd@F.YearAbsolute        = enumElement (UpdateCommand cmd) { title: "Year" } (year t)
renderCommand t cmd@F.MonthFull           = choiceElement (UpdateCommand cmd) { title: "Month" } (month t)
renderCommand t cmd@F.MonthShort          = choiceElement (UpdateCommand cmd) { title: "Month" } (monthShort t)
renderCommand t cmd@F.MonthTwoDigits      = enumElement (UpdateCommand cmd) { title: "Month" } (month t)
renderCommand t cmd@F.DayOfMonthTwoDigits = enumElement (UpdateCommand cmd) { title: "Day" } (day t)
renderCommand t cmd@F.DayOfMonth          = enumElement (UpdateCommand cmd) { title: "Day" } (day t)
renderCommand _ (F.Placeholder str)       = textElement { text: str}


-- TODO switch to Validation/Either instead of Maybe to
-- show helpful error messages instead of swallowing them.
evalDate ∷ ∀ m . DateQuery ~> DSL m
evalDate (UpdateCommand command val next) = do
  {date} <- H.get
  let date' = Int.fromString val >>= \n -> updateTime command n date

  case date' of
    Just date'' -> do
      H.modify _{ date = date'' }
      H.raise (NotifyChange date'')
    Nothing -> D.traceAnyA {val, command, msg: "parsing val or updating date has failed"}
  pure next

updateTime :: F.Command -> Int -> Date -> Maybe Date
updateTime F.YearFull n t = (toEnum n) >>= (_ `setYear4` t)
updateTime F.YearTwoDigits n t = (toEnum n ) >>= (_ `setYear2` t)
updateTime F.YearAbsolute n t = (toEnum n) >>= (_ `setYear` t)
updateTime F.MonthFull n t = (toEnum n) >>= (_ `setMonth` t)
updateTime F.MonthShort n t = (toEnum n) >>= (_ `setMonth` t)
updateTime F.MonthTwoDigits n t = (toEnum n) >>= (_ `setMonth` t)
updateTime F.DayOfMonthTwoDigits n t = (toEnum n) >>= (_ `setDay` t)
updateTime F.DayOfMonth n t = (toEnum n) >>= (_ `setDay` t)
updateTime (F.Placeholder _) _ t = pure t


evalPicker ∷ ∀ m . (PickerQuery Date) ~> DSL m
evalPicker (SetValue date next) = do
  H.modify _{ date = date }
  -- TODO this pattern will cause loop when parent changes value on once childe
  -- reaisis NotifyChange we should not raise this on SetValue or add a flag
  --  indicating that it was changed from ui or from parent
  -- H.raise (NotifyChange date)
  pure next
evalPicker (GetValue next) = do
  H.gets _.date <#> next
