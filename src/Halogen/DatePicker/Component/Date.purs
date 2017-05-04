module Halogen.Datapicker.Component.Date where

import Prelude
import Debug.Trace as D

import Halogen.Datapicker.Component.Types (PickerQuery(..), PickerMessage(..))
import Data.Date
  ( Date
  , Year, Month, Day
  , year, month, day
  , exactDate
  )
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap)
import Data.Foldable (foldMap)
import Data.Unfoldable (unfoldr)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Enum
  ( class Enum
  , pred
  , succ
  , class BoundedEnum
  , Cardinality(..)
  , cardinality
  , toEnum
  , fromEnum)
import Data.Functor.Coproduct (Coproduct, coproduct, right)
import Halogen.Datapicker.Component.Date.Format as F
import Data.String as Str
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.Int as Int

data DateQuery a = UpdateCommand F.Command String a

type Query = Coproduct (PickerQuery Date) DateQuery
type Message = PickerMessage Date
type State =
  { format :: F.Format
  , date :: Date
  }

type DSL = H.ComponentDSL State Query Message
type HTML = H.ComponentHTML DateQuery

initialStateFromFormat ∷ F.Format -> State
initialStateFromFormat format = {format: format, date: bottom}

picker ∷ ∀ m. F.Format -> H.Component HH.HTML Query Unit Message m
picker fmt = H.component
  { initialState: const $ initialStateFromFormat fmt
  , render: render <#> (map right)
  , eval: coproduct evalPicker evalDate
  , receiver: const Nothing
  }
  where
  render ∷ State -> HTML
  render {date, format} = HH.ul_ $ foldMap (pure <<< f) (unwrap format)
    where
    f cmd = HH.li_ [renderCommand date cmd]

-- NOTE
-- maybe we can splitt `___Element cmd {___} (____ t)`
-- in two patrs two pats:
-- 1) takes `Command` and returns coresponding function waiting for selected value (Command -> x -> HTML)
-- 2) takes `Command` and `Date` and returns value which must be selected (Command -> Date -> x)
-- and here both of them are combined.
-- we might need to remove replace `BoundedEnum a` with `Int` in `choiseElement`

renderCommand :: Date -> F.Command -> HTML
renderCommand t cmd@F.YearFull            = numberElement cmd { title: "Year", min: 0, max: 9999} (fromEnum $ year t)
renderCommand t cmd@F.YearTwoDigits       = numberElement cmd { title: "Year", min: 0, max: 99} (year99 $ year t)
renderCommand t cmd@F.YearAbsolute        = numberElement cmd { title: "Year", min: fromEnum (bottom :: Year) , max: fromEnum (top :: Year)} (fromEnum $ year t)
renderCommand t cmd@F.MonthFull           = choiseElement cmd { title: "Month" } (month t)
renderCommand t cmd@F.MonthShort          = choiseElement cmd { title: "Month" } (MonthShort $ month t)
renderCommand t cmd@F.MonthTwoDigits      = numberElement cmd { title: "Month", min: 1, max: 12} (fromEnum $ month t)
renderCommand t cmd@F.DayOfMonthTwoDigits = numberElement cmd { title: "Day", min: 1, max: 31} (fromEnum $ day t)
renderCommand t cmd@F.DayOfMonth          = numberElement cmd { title: "Day", min: 1, max: 31} (fromEnum $ day t)
renderCommand _ (F.Placeholder str)       = textElement { text: str}


year99 :: Year -> Int
year99 = fromEnum >>> \y -> y - (y `unPrecise` 100)

-- > 123456789 `unPrecise` 1000
-- 123456000
unPrecise :: Int -> Int -> Int
unPrecise n by = n / by * by


numberElement :: F.Command -> {title :: String, min :: Int, max :: Int} -> Int -> HTML
numberElement cmd {title, min, max} value = HH.input
  [ HP.type_ HP.InputNumber
  , HP.title title
  , HP.value (show value)
  , HP.min (Int.toNumber min)
  , HP.max (Int.toNumber max)
  , HE.onValueChange (HE.input (UpdateCommand cmd))
  ]

choiseElement :: forall a. Show a => BoundedEnum a => F.Command -> {title :: String } -> a -> HTML
choiseElement cmd {title} val = HH.select
  [ HE.onValueChange (HE.input (UpdateCommand cmd))
  , HP.title title
  ] (values <#> renderVal)
  where
  values = unfoldr genValues 0
  genValues n = toEnum n <#> \a -> Tuple a (n + 1)

  renderVal val' = HH.option
    [ HP.value $ show $ fromEnum val'
    , HP.selected (val' == val)
    ]
    [ HH.text $ show val' ]


textElement :: {text :: String} -> HTML
textElement {text} = HH.span_ [HH.text text]

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
updateTime F.YearFull n t = (toEnum n) >>= (_ `setYear` t)
updateTime F.YearTwoDigits n t = (toEnum $ changeYear n ) >>= (_ `setYear` t)
  where
  changeYear y99 = ((fromEnum $ year t) `unPrecise` 100) + y99
updateTime F.YearAbsolute n t = (toEnum n) >>= (_ `setYear` t)
updateTime F.MonthFull n t = (toEnum n) >>= (_ `setMonth` t)
updateTime F.MonthShort n t = (toEnum n) >>= (_ `setMonth` t)
updateTime F.MonthTwoDigits n t = (toEnum n) >>= (_ `setMonth` t)
updateTime F.DayOfMonthTwoDigits n t = (toEnum n) >>= (_ `setDay` t)
updateTime F.DayOfMonth n t = (toEnum n) >>= (_ `setDay` t)
updateTime (F.Placeholder _) _ t = pure t

setYear :: Year -> Date -> Maybe Date
setYear a d = exactDate a (month d) (day d)

setMonth :: Month -> Date -> Maybe Date
setMonth a d = exactDate (year d) a (day d)

setDay :: Day -> Date -> Maybe Date
setDay a d = exactDate (year d) (month d) a

evalPicker ∷ ∀ m . (PickerQuery Date) ~> DSL m
evalPicker (SetValue date next) = do
  H.modify _{ date = date }
  H.raise (NotifyChange date)
  pure next
evalPicker (GetValue next) = do
  H.gets _.date <#> next

-- TODO monve this instanced to Data.Formatters.DateTime

newtype MonthShort = MonthShort Month
derive instance monthShortNewtype :: Newtype MonthShort _
derive instance monthShortGeneric :: Generic MonthShort _
derive instance monthShortEq :: Eq MonthShort
derive instance monthShortOrd :: Ord MonthShort

instance monthShortBounded :: Bounded MonthShort where
  bottom = MonthShort bottom
  top = MonthShort top

instance monthShortEnum :: Enum MonthShort where
  pred (MonthShort m) = pred m <#> MonthShort
  succ (MonthShort m) = succ m <#> MonthShort

instance monthShortBoundedEnum :: BoundedEnum MonthShort where
  cardinality = Cardinality $ unwrap (cardinality :: Cardinality Month)
  toEnum n = toEnum n <#> MonthShort
  fromEnum (MonthShort m) = fromEnum m

instance monthShortShow :: Show MonthShort where
  show (MonthShort m) = Str.take 3 $ show m
