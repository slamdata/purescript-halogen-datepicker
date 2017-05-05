module Halogen.Datapicker.Component.Time where

import Prelude
import Debug.Trace as D

import Halogen.Datapicker.Component.Types (PickerQuery(..), PickerMessage(..))
import Data.Time
  ( Time, Hour, Millisecond
  , second, minute, hour, millisecond
  , setSecond, setMinute, setHour, setMillisecond
  )
import Data.Newtype (unwrap)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), toEnum, fromEnum)
import Data.Functor.Coproduct (Coproduct, coproduct, right)
import Halogen.Datapicker.Component.Time.Format as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.Int as Int

data TimeQuery a = UpdateCommand F.Command String a

type Query = Coproduct (PickerQuery Time) TimeQuery
type Message = PickerMessage Time
type State =
  { format :: F.Format
  , time :: Time
  }

type DSL = H.ComponentDSL State Query Message
type HTML = H.ComponentHTML TimeQuery

initialStateFromFormat ∷ F.Format -> State
initialStateFromFormat format = {format: format, time: bottom}

picker ∷ ∀ m. F.Format -> H.Component HH.HTML Query Unit Message m
picker fmt = H.component
  { initialState: const $ initialStateFromFormat fmt
  , render: render <#> (map right)
  , eval: coproduct evalPicker evalTime
  , receiver: const Nothing
  }
  where
  render ∷ State -> HTML
  render {time, format} = HH.ul_ $ foldMap (pure <<< f) (unwrap format)
    where
    f cmd = HH.li_ [renderCommand time cmd]

-- NOTE
-- maybe we can splitt `___Element cmd {___} (____ t)`
-- in two patrs two pats:
-- 1) takes `Command` and returns coresponding function waiting for selected value (Command -> x -> HTML)
-- 2) takes `Command` and `Time` and returns value which must be selected (Command -> Time -> x)
-- and here both of them are combined.
-- we might need to remove replace `BoundedEnum a` with `Int` in `choiseElement`

renderCommand :: Time -> F.Command -> HTML
renderCommand t cmd@F.Hours24               = numberElement cmd { title: "Hours", min: 0, max: 23} (fromEnum $ hour t)
renderCommand t cmd@F.Hours12               = numberElement cmd { title: "Hours", min: 0, max: 11} (hour12 $ hour t)
renderCommand t cmd@F.Meridiem              = choiseElement cmd { title: "Meridiem", values: [Tuple AM "AM", Tuple PM "PM"]} (meridiem $ hour t)
renderCommand t cmd@F.MinutesTwoDigits      = numberElement cmd { title: "Minutes", min: 0, max: 59} (fromEnum $ minute t)
renderCommand t cmd@F.Minutes               = numberElement cmd { title: "Minutes", min: 0, max: 59} (fromEnum $ minute t)
renderCommand t cmd@F.SecondsTwoDigits      = numberElement cmd { title: "Seconds", min: 0, max: 59} (fromEnum $ second t)
renderCommand t cmd@F.Seconds               = numberElement cmd { title: "Seconds", min: 0, max: 59} (fromEnum $ second t)
renderCommand t cmd@F.Milliseconds          = numberElement cmd { title: "Milliseconds", min: 0, max: 999} (fromEnum $ millisecond t)
renderCommand t cmd@F.MillisecondsTwoDigits = numberElement cmd { title: "Milliseconds", min: 0, max: 99} (millisecond2 $ millisecond t)
renderCommand t cmd@F.MillisecondsShort     = numberElement cmd { title: "Milliseconds", min: 0, max: 9} (millisecond1 $ millisecond t)
renderCommand _ (F.Placeholder str)         = textElement { text: str}

hour12 :: Hour -> Int
hour12 = fromEnum >>> \h -> if h >= 12 then h - 12 else h

meridiem :: Hour -> Meridiem
meridiem = fromEnum >>> \h -> if h >= 12 then PM else AM

millisecond2 :: Millisecond -> Int
millisecond2 = fromEnum >>> (_ / 10)

millisecond1 :: Millisecond -> Int
millisecond1 = fromEnum >>> (_ / 100)

numberElement :: F.Command -> {title :: String, min :: Int, max :: Int} -> Int -> HTML
numberElement cmd {title, min, max} value = HH.input
  [ HP.type_ HP.InputNumber
  , HP.title title
  , HP.value (show value)
  , HP.min (Int.toNumber min)
  , HP.max (Int.toNumber max)
  , HE.onValueChange (HE.input (UpdateCommand cmd))
  ]

choiseElement :: forall a. BoundedEnum a => F.Command -> {title :: String, values :: Array (Tuple a String)} -> a -> HTML
choiseElement cmd {title, values} val = HH.select
  [ HE.onValueChange (HE.input (UpdateCommand cmd))
  , HP.title title
  ] (values <#> renderVal)
  where
  renderVal (Tuple val' text) = HH.option
    [ HP.value $ show $ fromEnum val'
    , HP.selected (val' == val)
    ]
    [ HH.text text ]


textElement :: {text :: String} -> HTML
textElement {text} = HH.span_ [HH.text text]

-- TODO switch to Validation/Either instead of Maybe to
-- show helpful error messages instead of swallowing them.
evalTime ∷ ∀ m . TimeQuery ~> DSL m
evalTime (UpdateCommand command val next) = do
  {time} <- H.get
  let time' = Int.fromString val >>= \n -> updateTime command n time

  case time' of
    Just time'' -> do
      H.modify _{ time = time'' }
      H.raise (NotifyChange time'')
    Nothing -> D.traceAnyA {val, command, msg: "parsing val or updating time has failed"}
  pure next

updateTime :: F.Command -> Int -> Time -> Maybe Time
updateTime F.Hours24 n t = setHour <$> toEnum n <*> pure t
updateTime F.Hours12 n t = setHour <$> toEnum (if (fromEnum $ hour t) < 12 then n else n + 12) <*> pure t
updateTime F.Meridiem n t = setHour <$> (toEnum n >>= changeMeridian) <*> pure t
  where
  h = fromEnum $ hour t
  changeMeridian m = toEnum $ case m of
    AM -> if h > 12 then h - 12 else h
    PM -> if h < 12 then h + 12 else h
updateTime F.MinutesTwoDigits n t = setMinute <$> (toEnum n) <*> pure t
updateTime F.Minutes n t = setMinute <$> (toEnum n) <*> pure t
updateTime F.SecondsTwoDigits n t = setSecond <$> (toEnum n) <*> pure t
updateTime F.Seconds n t = setSecond <$> (toEnum n) <*> pure t
updateTime F.Milliseconds n t = setMillisecond <$> (toEnum n) <*> pure t
updateTime F.MillisecondsTwoDigits n t = setMillisecond <$> (toEnum $ n * 10) <*> pure t
updateTime F.MillisecondsShort n t = setMillisecond <$> (toEnum $ n * 100) <*> pure t
updateTime (F.Placeholder _) _ t = pure t

evalPicker ∷ ∀ m . (PickerQuery Time) ~> DSL m
evalPicker (SetValue time next) = do
  H.modify _{ time = time }
  H.raise (NotifyChange time)
  pure next
evalPicker (GetValue next) = do
  H.gets _.time <#> next

-- TODO monve this instanced to Data.Formatters.DateTime
data Meridiem = AM | PM
derive instance meridiemEq ∷ Eq Meridiem
derive instance meridiemOrd ∷ Ord Meridiem
instance meridiemShow ∷ Show Meridiem where
  show AM = "AM"
  show PM = "PM"

instance meridiemBounded ∷ Bounded Meridiem where
  bottom = AM
  top = PM

instance meridiemEnum ∷ Enum Meridiem where
  pred PM = Just AM
  pred _ = Nothing
  succ AM = Just PM
  succ _ = Nothing

instance meridiemBoundedEnum ∷ BoundedEnum Meridiem where
  cardinality = Cardinality 2
  toEnum 1 = Just AM
  toEnum 2 = Just PM
  toEnum _ = Nothing
  fromEnum AM = 1
  fromEnum PM = 2
