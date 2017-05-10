module Example where

import Prelude
import Debug.Trace as D
import Data.Monoid (mempty)
import Data.Maybe (Maybe(..), fromJust)
import Data.Map (Map, lookup, insert)
import Data.Time (Time)
import Data.DateTime (DateTime)
import Data.Date (Date, canonicalDate)
import Data.Enum (class BoundedEnum, toEnum)
import Data.Either (Either(..), either)
import Data.Either.Nested (Either3)
import Data.Functor.Coproduct (left)
import Data.Functor.Coproduct.Nested as Coproduct
import Halogen.Component.ChildPath as CP

import Halogen.Datapicker.Component.Time.Format as TimeF
import Halogen.Datapicker.Component.Time as Time
import Halogen.Datapicker.Component.Date.Format as DateF
import Halogen.Datapicker.Component.Date as Date
import Halogen.Datapicker.Component.DateTime.Format as DateTimeF
import Halogen.Datapicker.Component.DateTime as DateTime
import Halogen.Datapicker.Component.Types (PickerQuery(..), PickerMessage(..))
import Partial.Unsafe (unsafePartial)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type TimeIdx = Int
type DateIdx = Int
type DateTimeIdx = Int
data Query a
  = Set SetPayload a
  | HandleMessage MessagePayload a

data SetPayload = SetTime Int Time | SetDate Int Date | SetDateTime Int DateTime
data MessagePayload = MsgTime Int Time.Message | MsgDate Int Date.Message | MsgDateTime Int DateTime.Message

type ChildQuery = Coproduct.Coproduct3 Time.Query Date.Query DateTime.Query

type State =
  { times :: Map TimeIdx String
  , dates :: Map DateIdx String
  , dateTimes :: Map DateIdx String
  }

type Slot = Either3 TimeIdx DateIdx DateTimeIdx

cpTime :: CP.ChildPath Time.Query ChildQuery TimeIdx Slot
cpTime = CP.cp1

cpDate :: CP.ChildPath Date.Query ChildQuery DateIdx Slot
cpDate = CP.cp2

cpDateTime :: CP.ChildPath DateTime.Query ChildQuery DateIdx Slot
cpDateTime = CP.cp3

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
  initialState = { times: mempty, dates: mempty, dateTimes: mempty}
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
    <> renderDate s 7 "YY:MM" (Right $ canonicalDate (enum 2017) (enum 1) (enum 1))
    <> renderDate s (-1) "YY:MM:MMM:YYYY mm:ss" (Left "---")

    <> [HH.h1_ [ HH.text "DateTime" ]]
    <> renderDateTime s 0 "YYYY:MM:DD HH:mm" (Left "2017:12:27 12:34")
    <> renderDateTime s 1 "HH:mm:ss,SSS-YYYY:MMM" (Left "13:45:49,119-2017:May")
    <> renderDateTime s (-1) "HH:mm:m:ss:SS,SaYY:MM:MMM:YYYY mm:ss" (Left "---")

  enum :: ∀ a. BoundedEnum a => Int -> a
  enum = unsafePartial fromJust <<< toEnum

  renderTime ∷ State -> Int -> String -> Either String Time -> Array (HTML m)
  renderTime s = renderExample timeConfig s.times

  renderDate ∷ State -> Int -> String -> Either String Date -> Array (HTML m)
  renderDate s = renderExample dateConfig s.dates

  renderDateTime ∷ State -> Int -> String -> Either String DateTime -> Array (HTML m)
  renderDateTime s = renderExample dateTimeConfig s.dateTimes

  eval ∷ Query ~> DSL m
  eval (Set payload next) = do
    void $ case payload of
      SetTime     idx val -> H.query' timeConfig.cp     idx $ H.action $ left <<< (SetValue val)
      SetDate     idx val -> H.query' dateConfig.cp     idx $ H.action $ left <<< (SetValue val)
      SetDateTime idx val -> H.query' dateTimeConfig.cp idx $ H.action $ left <<< (SetValue val)
    pure next
  eval (HandleMessage payload next) = do
    case payload of
      MsgTime     idx (NotifyChange val) -> H.modify \s -> s {times     = insert idx (show val) s.times}
      MsgDate     idx (NotifyChange val) -> H.modify \s -> s {dates     = insert idx (show val) s.dates}
      MsgDateTime idx (NotifyChange val) -> H.modify \s -> s {dateTimes = insert idx (show val) s.dateTimes}
    pure next


type ExampleConfig input fmt query out m =
  { mkFormat :: String -> Either String fmt
  , unformat :: fmt -> String -> Either String input
  -- , picker :: fmt -> H.Component HH.HTML query input out m
  , picker :: fmt -> H.Component HH.HTML query Unit out m
  , handler :: ∀z. Int -> out -> z -> Query z
  , setter :: ∀z. Int -> input -> z -> Query z
  , cp :: CP.ChildPath query ChildQuery Int Slot
  }

renderExample :: ∀ input fmt query out m
  . ExampleConfig input fmt query out m
  -> Map Int String
  -> Int
  -> String
  -> Either String input
  -> Array (HTML m)
renderExample c items idx fmt' value'= unEither $ do
  fmt <- c.mkFormat fmt'
  value <- either (c.unformat fmt) Right value'
  let cmp = (c.picker fmt)
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

timeConfig :: ∀ m. ExampleConfig Time TimeF.Format Time.Query Time.Message m
timeConfig =
  { mkFormat: TimeF.fromString
  , unformat: TimeF.unformat
  , picker: Time.picker
  , handler: \idx msg -> HandleMessage (MsgTime idx msg)
  , setter: \idx val -> Set (SetTime idx val)
  , cp: cpTime
  }

dateConfig :: ∀ m. ExampleConfig Date DateF.Format Date.Query Date.Message m
dateConfig =
  { mkFormat: DateF.fromString
  , unformat: DateF.unformat
  , picker: Date.picker
  , handler: \idx msg -> HandleMessage (MsgDate idx msg)
  , setter: \idx val -> Set (SetDate idx val)
  , cp: cpDate
  }

dateTimeConfig :: ∀ m. ExampleConfig DateTime DateTimeF.Format DateTime.Query DateTime.Message m
dateTimeConfig =
  { mkFormat: DateTimeF.fromString
  , unformat: DateTimeF.unformat
  , picker: DateTime.picker
  , handler: \idx msg -> HandleMessage (MsgDateTime idx msg)
  , setter: \idx val -> Set (SetDateTime idx val)
  , cp: cpDateTime
  }
