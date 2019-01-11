module Main where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Data.Bitraversable (bitraverse)
import Data.Date (Date, canonicalDate)
import Data.DateTime (DateTime(..))
import Data.Either (Either(..), either, fromRight)
import Data.Enum (class BoundedEnum, toEnum)
import Data.Foldable (fold)
import Data.Formatter.Interval (unformatInterval)
import Data.Interval (Interval(..))
import Data.Interval as I
import Data.Interval.Duration.Iso (IsoDuration, mkIsoDuration)
import Data.Map (Map, lookup, insert)
import Data.Maybe (Maybe(..), fromJust)
import Data.Maybe.Last (Last(..))
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Time (Time, setHour, setMinute)
import Effect (Effect)
import Effect.Exception as Ex
import Halogen as H
import Halogen.Aff as HA
import Halogen.Datepicker.Component.Date as Date
import Halogen.Datepicker.Component.DateTime as DateTime
import Halogen.Datepicker.Component.Duration as Duration
import Halogen.Datepicker.Component.Interval as Interval
import Halogen.Datepicker.Component.Time as Time
import Halogen.Datepicker.Component.Types (PickerMessage(..), setValue)
import Halogen.Datepicker.Config (Config(..), defaultConfig)
import Halogen.Datepicker.Format.Date as DateF
import Halogen.Datepicker.Format.DateTime as DateTimeF
import Halogen.Datepicker.Format.Duration as DurationF
import Halogen.Datepicker.Format.Interval as IntervalF
import Halogen.Datepicker.Format.Time as TimeF
import Halogen.Datepicker.Internal.Utils (mustBeMounted)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Partial.Unsafe (unsafePartial)
import Prim.Row as Row

type TimeIdx = Int
type DateIdx = Int
type DateTimeIdx = Int
type DurationIdx = Int
type IntervalIdx = Int

data Query a
  = Set SetPayload a
  | HandleMessage MessagePayload a

data SetPayload
  = SetTime TimeIdx (Maybe Time)
  | SetDate DateIdx (Maybe Date)
  | SetDateTime DateTimeIdx (Maybe DateTime)
  | SetDuration DurationIdx (Maybe IsoDuration)
  | SetInterval IntervalIdx (Maybe (Interval IsoDuration DateTime))

data MessagePayload
  = MsgTime TimeIdx Time.Message
  | MsgDate DateIdx Date.Message
  | MsgDateTime DateTimeIdx DateTime.Message
  | MsgDuration DurationIdx Duration.Message
  | MsgInterval IntervalIdx Interval.Message

type State =
  { times ∷ Map TimeIdx String
  , dates ∷ Map DateIdx String
  , dateTimes ∷ Map DateTimeIdx String
  , durations ∷ Map DurationIdx String
  , intervals ∷ Map IntervalIdx String
  }

type Slots =
  ( time ∷ Time.Slot TimeIdx
  , date ∷ Date.Slot DateIdx
  , dateTime ∷ DateTime.Slot DateTimeIdx
  , duration ∷ Duration.Slot DurationIdx
  , interval ∷ Interval.Slot IntervalIdx
  )

_time = SProxy ∷ SProxy "time"
_date = SProxy ∷ SProxy "date"
_dateTime = SProxy ∷ SProxy "dateTime"
_duration = SProxy ∷ SProxy "duration"
_interval = SProxy ∷ SProxy "interval"

type HTML m = H.ComponentHTML Query Slots m
type DSL m = H.HalogenM State Query Slots Void m

main ∷ Effect Unit
main = HA.runHalogenAff do
  body ← HA.awaitBody
  runUI example unit body

type StrOr = Either String

example
  ∷ ∀ m
  . MonadError Ex.Error m
  ⇒ Applicative m
  ⇒ H.Component HH.HTML Query Unit Void m
example =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Nothing
    , finalizer: Nothing
    }
  where
  initialState =
    { times: mempty
    , dates: mempty
    , dateTimes: mempty
    , durations: mempty
    , intervals: mempty
    }
  render ∷ State → HTML m
  render s = HH.div_
    $  [HH.h1_ [ HH.text "Time" ]]
    <> renderTime s 0 "HH:mm" (Left "13:45")
    <> renderTime s 1 "HH:mm:ss,SSS" (Left "13:45:49,119")
    <> renderTime s 2 "mm:ss,SSS" (Left "45:49,119")
    <> renderTime s 3 "mm:ss,SSS" (Left "45:49,119")
    <> renderTime s 4 "mm:ss,SS" (Left "45:49,11")
    <> renderTime s 5 "mm:ss,S" (Left "45:49,1")
    <> renderTime s 6 "a hh:mm:ss,SSS" (Left "PM 02:45:49,119")
    <> renderTime s (-1) "HH:mm:m:ss:SS,Sa" (Left "---")

    <> [HH.h1_ [ HH.text "Date" ]]
    <> renderDate s 0 "YYYY:MM:DD" (Left "2017:12:27")
    <> renderDate s 1 "YYYY:MM" (Left "2017:12")
    <> renderDate s 2 "YYYY" (Left "2017")
    <> renderDate s 3 "YYYY:MMM" (Left "2017:May")
    <> renderDate s 4 "YYYY:MMMM" (Left "2017:May")
    <> renderDate s 5 "Y:MM" (Left "39017:12")
    <> renderDate s 6 "YY:MM" (Left "17:12")
    <> renderDate s 7 "YY:MM" (Right $ testDate)
    <> renderDate s (-1) "YY:MM:MMM:YYYY mm:ss" (Left "---")

    <> [HH.h1_ [ HH.text "DateTime" ]]
    <> renderDateTime s 0 "YYYY:MM:DD HH:mm" (Left "2017:12:27 12:34")
    <> renderDateTime s 1 "HH:mm:ss,SSS-YYYY:MMM" (Left "13:45:49,119-2017:May")
    <> renderDateTime s (-1) "HH:mm:m:ss:SS,SaYY:MM:MMM:YYYY mm:ss" (Left "---")

    <> [HH.h1_ [ HH.text "Duration" ]]
    <> renderDuration s 0
        [ DurationF.Year
        , DurationF.Month
        , DurationF.Day
        ]
        (Right testDuration)
    <> renderDuration s 1
        [ DurationF.Year
        , DurationF.Month
        , DurationF.Day
        , DurationF.Hour
        , DurationF.Minute
        , DurationF.Second
        ]
        (Right testDuration)
    <> [HH.h1_ [ HH.text "Interval" ]]
    <> renderInterval s 0
        (DurationOnly
          [ DurationF.Year
          , DurationF.Month
          , DurationF.Day
          ]
        )
        (Right $ DurationOnly testDuration)
    <> renderInterval s 1
        (StartDuration
          "YYYY:MM:DD"
          [ DurationF.Year
          , DurationF.Month
          , DurationF.Day
          ]
        )
        (Right $ StartDuration testDateTime testDuration)
    <> renderInterval s 2
        (DurationEnd
          [ DurationF.Year
          , DurationF.Month
          , DurationF.Day
          , DurationF.Hour
          , DurationF.Minute
          , DurationF.Second
          ]
          "YYYY:MMM:DD-HH:mm"
        )
        (Right $ DurationEnd testDuration testDateTime)
    <> renderInterval s 3
        (StartEnd "YYYY:MM:DD" "YYYY:MM:DD")
        (Right $ StartEnd testDateTime testDateTime)

  testDate ∷ Date
  testDate = canonicalDate (enum 2017) (enum 1) (enum 1)

  testDateTime ∷ DateTime
  testDateTime = DateTime testDate (bottom # setHour (enum 2) # setMinute (enum 2))

  testDuration ∷ IsoDuration
  testDuration = unsafePartial fromRight -- this duration must be valid
    $ mkIsoDuration
    $ fold
      [ I.year 100.0
      , I.month 25.0
      , I.day 245.0
      , I.hour 0.0
      , I.minute 100.0
      , I.second 124.0
      ]

  enum ∷ ∀ a. BoundedEnum a ⇒ Int → a
  enum = unsafePartial fromJust <<< toEnum -- Ints passed to this func must be in range

  renderTime ∷ State → Int → String → StrOr Time → Array (HTML m)
  renderTime s = renderExample timeConfig _time s.times

  renderDate ∷ State → Int → String → StrOr Date → Array (HTML m)
  renderDate s = renderExample dateConfig _date s.dates

  renderDuration
    ∷ State
    → Int
    → Array DurationF.Command
    → StrOr IsoDuration
    → Array (HTML m)
  renderDuration s = renderExample durationConfig _duration s.durations

  renderInterval
    ∷ State
    → Int
    → Interval (Array DurationF.Command) String
    → StrOr (Interval IsoDuration DateTime)
    → Array (HTML m)
  renderInterval s = renderExample intervalConfig _interval s.intervals

  renderDateTime
    ∷ State
    → Int
    → String
    → StrOr DateTime
    → Array (HTML m)
  renderDateTime s = renderExample dateTimeConfig _dateTime s.dateTimes

  eval ∷ Query ~> DSL m
  eval (Set payload next) = do
    mustBeMounted =<< case payload of
      SetTime idx val →
        H.query _time idx $ setValue $ map Right val
      SetDate idx val →
        H.query _date idx $ setValue $ map Right val
      SetDateTime idx val →
        H.query _dateTime idx $ setValue $ map Right val
      SetDuration idx val →
        H.query _duration idx $ setValue $ map Right val
      SetInterval idx val →
        map void $ H.query _interval idx $ setValue $ map Right val
    pure next
  eval (HandleMessage payload next) = do
    case payload of
      MsgTime idx (NotifyChange val) →
        H.modify_ \s → s{ times = insert idx (show val) s.times }
      MsgDate idx (NotifyChange val) →
        H.modify_ \s → s{ dates = insert idx (show val) s.dates }
      MsgDateTime idx (NotifyChange val) →
        H.modify_ \s → s{ dateTimes = insert idx (show val) s.dateTimes }
      MsgDuration idx (NotifyChange val) →
        H.modify_ \s → s{ durations = insert idx (show val) s.durations }
      MsgInterval idx (NotifyChange val) →
        H.modify_ \s → s{ intervals = insert idx (show val) s.intervals }
    pure next

type ExampleConfig fmtInput input fmt query out m =
  { mkFormat ∷ fmtInput → StrOr fmt
  , unformat ∷ fmt → String → StrOr input
  , picker ∷ fmt → H.Component HH.HTML query Unit out m
  , handler ∷ ∀ z. Int → out → z → Query z
  , setter ∷ ∀ z. Int → Maybe input → z → Query z
  }

renderExample
  ∷ ∀ fmtInput input fmt sym query out px m
  . Row.Cons sym (H.Slot query out Int) px Slots
  ⇒ IsSymbol sym
  ⇒ ExampleConfig fmtInput input fmt query out m
  → SProxy sym
  → Map Int String
  → Int
  → fmtInput
  → StrOr input
  → Array (HTML m)
renderExample c sp items idx fmt' value'= unEither $ do
  fmt ← c.mkFormat fmt'
  value ← either (c.unformat fmt) Right value'
  let cmp = c.picker fmt
  pure
    [ HH.slot sp idx cmp unit (HE.input (c.handler idx))
    , btn (Just value) "reset"
    , btn Nothing "clear"
    , case lookup idx items of
        Nothing → HH.div_ [HH.text "no value is set"]
        Just val → HH.div_ [HH.text $ "value: " <> val]
    ]
  where
  btn ∷ Maybe input → String → HTML m
  btn val txt = HH.button
    [ HE.onClick $ HE.input_ $ c.setter idx val]
    [ HH.text txt]
  unEither ∷ StrOr (Array (HTML m)) → Array (HTML m)
  unEither = either (HH.text >>> pure >>> HH.div_ >>> pure) identity

timeConfig
  ∷ ∀ m
  . MonadError Ex.Error m
  ⇒ ExampleConfig String Time TimeF.Format Time.Query Time.Message m
timeConfig =
  { mkFormat: TimeF.fromString
  , unformat: TimeF.unformat
  , picker: Time.picker
  , handler: \idx msg → HandleMessage (MsgTime idx msg)
  , setter: \idx val → Set (SetTime idx val)
  }

dateConfig
  ∷ ∀ m
  . MonadError Ex.Error m
  ⇒ ExampleConfig String Date DateF.Format Date.Query Date.Message m
dateConfig =
  { mkFormat: DateF.fromString
  , unformat: DateF.unformat
  , picker: Date.picker
  , handler: \idx msg → HandleMessage (MsgDate idx msg)
  , setter: \idx val → Set (SetDate idx val)
  }

dateTimeConfig
  ∷ ∀ m
  . MonadError Ex.Error m
  ⇒ ExampleConfig String DateTime DateTimeF.Format DateTime.Query DateTime.Message m
dateTimeConfig =
  { mkFormat: DateTimeF.fromString
  , unformat: DateTimeF.unformat
  , picker: DateTime.picker
  , handler: \idx msg → HandleMessage (MsgDateTime idx msg)
  , setter: \idx val → Set (SetDateTime idx val)
  }

durationConfig
  ∷ ∀ m
  . MonadError Ex.Error m
  ⇒ ExampleConfig (Array DurationF.Command) IsoDuration DurationF.Format Duration.Query Duration.Message m
durationConfig =
  { mkFormat: DurationF.mkFormat
  , unformat: const DurationF.unformat
  , picker: Duration.picker
  , handler: \idx msg → HandleMessage (MsgDuration idx msg)
  , setter: \idx val → Set (SetDuration idx val)
  }

intervalConfig
  ∷ ∀ m
  . MonadError Ex.Error m
  ⇒ ExampleConfig (Interval (Array DurationF.Command) String) (Interval IsoDuration DateTime) IntervalF.Format Interval.Query Interval.Message m
intervalConfig =
  { mkFormat: bitraverse DurationF.mkFormat DateTimeF.fromString
  , unformat: const unformatInterval
  , picker: Interval.pickerWithConfig (defaultConfig <> myConfig)
  , handler: \idx msg → HandleMessage (MsgInterval idx msg)
  , setter: \idx val → Set (SetInterval idx val)
  }

myConfig ∷ Config
myConfig = Config
  { root: [HH.ClassName "MyPicker"]
  , rootInvalid: [HH.ClassName "MyPicker--invalid"]
  , component: [HH.ClassName "MyPicker-component"]
  , placeholder: [HH.ClassName "MyPicker-placeholder"]
  , choice: [HH.ClassName "MyPicker-input"]
  , choiceEmptyTitle: Last $ Just "=="
  , input: [HH.ClassName "MyPicker-input"]
  , inputInvalid: [HH.ClassName "MyPicker-input--invalid"]
  , inputLength: \n → [ HH.ClassName $ "MyPicker-input--length-" <> show n]
  }
