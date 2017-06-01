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
import Data.Date (Date, day, month, year)
import Data.Newtype (unwrap)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Functor.Coproduct (Coproduct, coproduct, right)
import Halogen.Datapicker.Component.Date.Format as F
import Halogen as H
import Halogen.HTML as HH
import Data.Int as Int

data DateQuery a = UpdateCommand F.Command String a

type Query = Coproduct (PickerQuery Unit Date) DateQuery
type Message = PickerMessage Date
type State =
  { format :: F.Format
  , date :: Date
  }

type DSL = H.ComponentDSL State Query Message
type HTML = H.ComponentHTML DateQuery

picker ∷ ∀ m. F.Format -> Date -> H.Component HH.HTML Query Unit Message m
picker format date = H.component
  { initialState: const {format, date}
  , render: render <#> (map right)
  , eval: coproduct evalPicker evalDate
  , receiver: const Nothing
  }

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


evalPicker ∷ ∀ m . (PickerQuery Unit Date) ~> DSL m
evalPicker (SetValue date next) = do
  H.modify _{ date = date }
  pure $ next unit
evalPicker (GetValue next) = H.gets _.date <#> next
