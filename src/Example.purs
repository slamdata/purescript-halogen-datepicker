module Example where

import Prelude
import Debug.Trace as D
import Data.Either.Nested as Either
import Data.Functor.Coproduct.Nested as Coproduct
import Data.Interval as I
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.Datapicker.Component.Date as Date
import Halogen.Datapicker.Component.Date.Format as DateF
import Halogen.Datapicker.Component.DateTime as DateTime
import Halogen.Datapicker.Component.DateTime.Format as DateTimeF
import Halogen.Datapicker.Component.Duration as Duration
import Halogen.Datapicker.Component.Duration.Format as DurationF
import Halogen.Datapicker.Component.Interval as Interval
import Halogen.Datapicker.Component.Interval.Format as IntervalF
import Halogen.Datapicker.Component.Time as Time
import Halogen.Datapicker.Component.Time.Format as TimeF
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Data.Bitraversable (bitraverse)
import Data.Date (Date, canonicalDate)
import Data.DateTime (DateTime(..))
import Data.Either (Either(..), either)
import Data.Enum (class BoundedEnum, toEnum)
import Data.Formatter.Interval (unformatInterval)
import Data.Functor.Coproduct (left)
import Data.Interval (Interval(..), IsoDuration)
import Data.Map (Map, lookup, insert)
import Data.Maybe (Maybe(..), fromJust)
import Data.Monoid (mempty)
import Data.Time (Time, setHour, setMinute)
import Halogen.Datapicker.Component.Types (PickerMessage(..), PickerQuery(..), mustBeMounted)
import Partial.Unsafe (unsafePartial, unsafePartialBecause)

type TimeIdx = Int
type DateIdx = Int
type DateTimeIdx = Int
type DurationIdx = Int
type IntervalIdx = Int

data Query a
  = Set SetPayload a
  | HandleMessage MessagePayload a

data SetPayload
  = SetTime Int Time
  | SetDate Int Date
  | SetDateTime Int DateTime
  | SetDuration Int IsoDuration
  | SetInterval Int (Interval IsoDuration DateTime)

data MessagePayload
  = MsgTime Int Time.Message
  | MsgDate Int Date.Message
  | MsgDateTime Int DateTime.Message
  | MsgDuration Int Duration.Message
  | MsgInterval Int Interval.Message

type ChildQuery = Coproduct.Coproduct5 Time.Query Date.Query DateTime.Query Duration.Query Interval.Query

type State =
  { times :: Map TimeIdx String
  , dates :: Map DateIdx String
  , dateTimes :: Map DateTimeIdx String
  , durations :: Map DurationIdx String
  , intervals :: Map IntervalIdx String
  }

type Slot = Either.Either5 TimeIdx DateIdx DateTimeIdx DurationIdx IntervalIdx

cpTime :: CP.ChildPath Time.Query ChildQuery TimeIdx Slot
cpTime = CP.cp1

cpDate :: CP.ChildPath Date.Query ChildQuery DateIdx Slot
cpDate = CP.cp2

cpDateTime :: CP.ChildPath DateTime.Query ChildQuery DateTimeIdx Slot
cpDateTime = CP.cp3

cpDuration :: CP.ChildPath Duration.Query ChildQuery DurationIdx Slot
cpDuration = CP.cp4

cpInterval :: CP.ChildPath Interval.Query ChildQuery IntervalIdx Slot
cpInterval = CP.cp5

type HTML m = H.ParentHTML Query ChildQuery Slot m
type DSL m = H.ParentDSL State Query ChildQuery Slot Void m



main ∷ forall m. Applicative m => H.Component HH.HTML Query Unit Void m
main =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
  initialState =
    { times: mempty
    , dates: mempty
    , dateTimes: mempty
    , durations: mempty
    , intervals: mempty
    }
  render ∷ State -> HTML m
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
        (JustDuration
          [ DurationF.Year
          , DurationF.Month
          , DurationF.Day
          ]
        )
        (Right $ JustDuration testDuration)
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
          "YYYY:MM:DD"
        )
        (Right $ DurationEnd testDuration testDateTime)
    <> renderInterval s 3
        (StartEnd "YYYY:MM:DD" "YYYY:MM:DD")
        (Right $ StartEnd testDateTime testDateTime)

  testDate :: Date
  testDate = canonicalDate (enum 2017) (enum 1) (enum 1)

  testDateTime :: DateTime
  testDateTime = DateTime testDate (bottom # setHour (enum 2) # setMinute (enum 2))

  testDuration :: IsoDuration
  testDuration = unsafePartial fromJust $ I.mkIsoDuration $ I.year 100.0 <> I.month 25.0 <> I.day 245.0 <> I.minute 100.0 <> I.second 124.0

  enum :: ∀ a. BoundedEnum a => Int -> a
  enum = unsafePartial fromJust <<< toEnum

  renderTime ∷ State -> Int -> String -> Either String Time -> Array (HTML m)
  renderTime s = renderExample timeConfig s.times

  renderDate ∷ State -> Int -> String -> Either String Date -> Array (HTML m)
  renderDate s = renderExample dateConfig s.dates

  renderDuration ∷ State -> Int -> Array DurationF.Command -> Either String IsoDuration -> Array (HTML m)
  renderDuration s = renderExample durationConfig s.durations

  renderInterval ∷ State -> Int -> Interval (Array DurationF.Command) String -> Either String (Interval IsoDuration DateTime) -> Array (HTML m)
  renderInterval s = renderExample intervalConfig s.intervals

  renderDateTime ∷ State -> Int -> String -> Either String DateTime -> Array (HTML m)
  renderDateTime s = renderExample dateTimeConfig s.dateTimes

  eval ∷ Query ~> DSL m
  eval (Set payload next) = do
    map mustBeMounted $ case payload of
      SetTime     idx val -> H.query' timeConfig.cp     idx $ H.request $ left <<< (SetValue val)
      SetDate     idx val -> H.query' dateConfig.cp     idx $ H.request $ left <<< (SetValue val)
      SetDateTime idx val -> H.query' dateTimeConfig.cp idx $ H.request $ left <<< (SetValue val)
      SetDuration idx val -> H.query' durationConfig.cp idx $ H.request $ left <<< (SetValue val)
      SetInterval idx val -> do
        res <- H.query' intervalConfig.cp idx $ H.request $ left <<< (SetValue val)
        pure $ void $ res <#> (\error ->  D.traceAny {message:"can't update interval", error})
    pure next
  eval (HandleMessage payload next) = do
    case payload of
      MsgTime     idx (NotifyChange val) -> H.modify \s -> s {times     = insert idx (show val) s.times}
      MsgDate     idx (NotifyChange val) -> H.modify \s -> s {dates     = insert idx (show val) s.dates}
      MsgDateTime idx (NotifyChange val) -> H.modify \s -> s {dateTimes = insert idx (show val) s.dateTimes}
      MsgDuration idx (NotifyChange val) -> H.modify \s -> s {durations = insert idx (show val) s.durations}
      MsgInterval idx (NotifyChange val) -> H.modify \s -> s {intervals = insert idx (show val) s.intervals}
    pure next



type ExampleConfig fmtInput input fmt query out m =
  { mkFormat :: fmtInput -> Either String fmt
  , unformat :: fmt -> String -> Either String input
  , picker :: fmt -> input -> H.Component HH.HTML query Unit out m
  , handler :: ∀z. Int -> out -> z -> Query z
  , setter :: ∀z. Int -> input -> z -> Query z
  , cp :: CP.ChildPath query ChildQuery Int Slot
  }

renderExample :: ∀ fmtInput input fmt query out m
  . ExampleConfig fmtInput input fmt query out m
  -> Map Int String
  -> Int
  -> fmtInput
  -> Either String input
  -> Array (HTML m)
renderExample c items idx fmt' value'= unEither $ do
  fmt <- c.mkFormat fmt'
  value <- either (c.unformat fmt) Right value'
  let cmp = c.picker fmt value
  pure
    [ HH.slot' c.cp idx cmp unit (HE.input (c.handler idx))
    , HH.button [ HE.onClick $ HE.input_ $ c.setter idx value] [ HH.text "reset"]
    , case lookup idx items of
        Nothing -> HH.div_ [HH.text "no value is set"]
        Just val -> HH.div_ [HH.text $ "value: " <> val]
    ]
  where
  unEither :: Either String (Array (HTML m)) -> Array (HTML m)
  unEither = either (HH.text >>> pure >>> HH.div_ >>> pure) id

timeConfig :: ∀ m. ExampleConfig String Time TimeF.Format Time.Query Time.Message m
timeConfig =
  { mkFormat: TimeF.fromString
  , unformat: TimeF.unformat
  , picker: Time.picker
  , handler: \idx msg -> HandleMessage (MsgTime idx msg)
  , setter: \idx val -> Set (SetTime idx val)
  , cp: cpTime
  }

dateConfig :: ∀ m. ExampleConfig String Date DateF.Format Date.Query Date.Message m
dateConfig =
  { mkFormat: DateF.fromString
  , unformat: DateF.unformat
  , picker: Date.picker
  , handler: \idx msg -> HandleMessage (MsgDate idx msg)
  , setter: \idx val -> Set (SetDate idx val)
  , cp: cpDate
  }

dateTimeConfig :: ∀ m. ExampleConfig String DateTime DateTimeF.Format DateTime.Query DateTime.Message m
dateTimeConfig =
  { mkFormat: DateTimeF.fromString
  , unformat: DateTimeF.unformat
  , picker: DateTime.picker
  , handler: \idx msg -> HandleMessage (MsgDateTime idx msg)
  , setter: \idx val -> Set (SetDateTime idx val)
  , cp: cpDateTime
  }

durationConfig :: ∀ m. ExampleConfig (Array DurationF.Command) IsoDuration DurationF.Format Duration.Query Duration.Message m
durationConfig =
  { mkFormat: DurationF.mkFormat
  , unformat: const DurationF.unformat
  , picker: Duration.picker
  , handler: \idx msg -> HandleMessage (MsgDuration idx msg)
  , setter: \idx val -> Set (SetDuration idx val)
  , cp: cpDuration
  }

intervalConfig :: ∀ m. ExampleConfig
  (Interval (Array DurationF.Command) String)
  (Interval IsoDuration DateTime)
  IntervalF.Format
  Interval.Query
  Interval.Message m
intervalConfig =
  { mkFormat: bitraverse DurationF.mkFormat DateTimeF.fromString
  , unformat: const unformatInterval
  , picker: Interval.picker
  , handler: \idx msg -> HandleMessage (MsgInterval idx msg)
  , setter: \idx val -> Set (SetInterval idx val)
  , cp: cpInterval
  }
