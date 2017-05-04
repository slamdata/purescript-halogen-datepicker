module Example where

import Prelude
import Debug.Trace as D
import Data.Monoid (mempty)
import Data.Maybe (Maybe(..))
import Data.Map (Map, lookup, insert)
import Data.Time (Time)
import Data.Date (Date)
import Data.Either as Either
import Data.Either.Nested as EitherN
import Data.Functor.Coproduct (right, left)
import Data.Functor.Coproduct.Nested as Coproduct
import Halogen.Component.ChildPath as CP

import Halogen.Datapicker.Component.Time.Format as TimeF
import Halogen.Datapicker.Component.Time as Time
import Halogen.Datapicker.Component.Date.Format as DateF
import Halogen.Datapicker.Component.Date as Date
import Halogen.Datapicker.Component.Types (PickerQuery(..), PickerMessage(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type TimeIdx = Int
type DateIdx = Int
data ExampleQuery a
  = SetTime TimeIdx Time a
  | HandleTimeMessage TimeIdx Time.Message a
  | SetDate DateIdx Date a
  | HandleDateMessage DateIdx Date.Message a

type ChildQuery = Coproduct.Coproduct2 Time.Query Date.Query

type State =
  { times :: Map TimeIdx String
  , dates :: Map DateIdx String
  }

type ExampleSlot = EitherN.Either2 TimeIdx DateIdx

type HTML m = H.ParentHTML ExampleQuery ChildQuery ExampleSlot m

main ∷ forall m. Applicative m => H.Component HH.HTML ExampleQuery Unit Void m
main =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
  initialState = { times: mempty, dates: mempty}
  render ∷ State -> HTML m
  render {times, dates} = HH.div_
    $  [HH.h1_ [ HH.text "Time" ]]
    <> (renderTime times 0 "HH:mm" "13:45")
    <> (renderTime times 1 "HH:mm:ss,SSS" "13:45:49,119")
    <> (renderTime times 2 "mm:ss,SSS" "45:49,119")
    <> (renderTime times 3 "mm:ss,SSS" "45:49,119")
    <> (renderTime times 4 "mm:ss,SS" "45:49,11")
    <> (renderTime times 5 "mm:ss,S" "45:49,1")
    <> (renderTime times 6 "a hh:mm:ss,SSS" "PM 02:45:49,119")
    <> (renderTime times (-1) "HH:mm:m:ss:SS,Sa" "---")

    <> [HH.h1_ [ HH.text "Date" ]]
    <> renderDate dates 0 "YYYY:MM:DD" "2017:12:27"
    <> renderDate dates 1 "YYYY:MM" "2017:12"
    <> renderDate dates 2 "YYYY" "2017"

  renderTime ∷ Map TimeIdx String -> TimeIdx -> String -> String -> Array (HTML m)
  renderTime times idx fmtStr timeStr = unEither $ do
    fmt <- TimeF.fromString fmtStr
    time <- TimeF.unformat fmt timeStr
    pure
      [ HH.slot' CP.cp1 idx (Time.picker fmt) unit (HE.input (HandleTimeMessage idx))
      , HH.button [ HE.onClick $ HE.input_ $ SetTime idx time] [ HH.text $ "set: " <> timeStr ]
      , case lookup idx times of
          Nothing -> HH.div_ [HH.text "no time is set"]
          Just currentTime -> HH.div_ [HH.text $ "time:" <> currentTime]
      ]
    where
    unEither :: Either.Either String (Array (HTML m)) -> Array (HTML m)
    unEither (Either.Left err) = [HH.div_ [HH.text err]]
    unEither (Either.Right a) = a


  renderDate ∷ Map DateIdx String -> DateIdx -> String -> String -> Array (HTML m)
  renderDate dates idx fmtStr dateStr = unEither $ do
    fmt <- DateF.fromString fmtStr
    date <- DateF.unformat fmt dateStr
    pure
      [ HH.slot' CP.cp2 idx (Date.picker fmt) unit (HE.input (HandleDateMessage idx))
      , HH.button [ HE.onClick $ HE.input_ $ SetDate idx date] [ HH.text $ "set: " <> dateStr ]
      , case lookup idx dates of
          Nothing -> HH.div_ [HH.text "no date is set"]
          Just currentDate -> HH.div_ [HH.text $ "date:" <> currentDate]
      ]
    where
    unEither :: Either.Either String (Array (HTML m)) -> Array (HTML m)
    unEither (Either.Left err) = [HH.div_ [HH.text err]]
    unEither (Either.Right a) = a


  eval ∷ ExampleQuery ~> H.ParentDSL State ExampleQuery ChildQuery ExampleSlot Void m
  eval (SetTime idx time next) = do
    isSet <- H.query' CP.cp1 idx $ H.action $ left <<< (SetValue time)
    -- isSet <- H.query (TimeSlot idx) $ H.action $ left $ left <<< (SetValue time)
    -- TODO check if isSet is Just Looks like it's `Maybe Unit`
    D.traceAnyA isSet
    pure next

  eval (HandleTimeMessage idx msg next) = do
    case msg of
      NotifyChange t -> H.modify \s -> s {times = insert idx (show t) s.times}
    pure next

  eval (SetDate idx date next) = do
    isSet <- H.query' CP.cp2 idx $ H.action $ left <<< (SetValue date)
    -- TODO check if isSet is Just Looks like it's `Maybe Unit`
    D.traceAnyA isSet
    pure next

  eval (HandleDateMessage idx msg next) = do
    case msg of
      NotifyChange t -> H.modify \s -> s {dates = insert idx (show t) s.dates}
    pure next
