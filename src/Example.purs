module Example where

import Prelude
import Debug.Trace as D
import Data.Monoid (mempty)
import Data.Maybe (Maybe(..))
import Data.Map (Map, lookup, insert)
import Data.Time (Time)
import Data.Either (Either(..))
import Data.Functor.Coproduct (left)
import Halogen.Datapicker.Component.Time.Format as TimeF
import Halogen.Datapicker.Component.Time as Time
-- import Halogen.Datapicker.Component.Date.Format as DateF
-- import Halogen.Datapicker.Component.Date as Date
import Halogen.Datapicker.Component.Types (PickerQuery(..), PickerMessage(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type TimeIdx = Int
-- type DateIdx = Int
data ExampleQuery a
  = SetTime TimeIdx Time a
  | HandleTimeMessage TimeIdx Time.Message a
  -- | SetDate DateIdx String a
  -- | HandleDateMessage DateIdx Time.Message a

type State =
  { times :: Map TimeIdx String
  }
data ExampleSlot = TimeSlot TimeIdx
derive instance eqExampleSlot ∷ Eq ExampleSlot
derive instance ordExampleSlot ∷ Ord ExampleSlot
type HTML m = H.ParentHTML ExampleQuery Time.Query ExampleSlot m
main ∷ forall m. Applicative m => H.Component HH.HTML ExampleQuery Unit Void m
main =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
  initialState = { times: mempty}
  render ∷ State -> HTML m
  render {times} = HH.div_
    $  [HH.h1_ [ HH.text "Time" ]]
    <> (renderTime times 0 "HH:mm" "13:45")
    <> (renderTime times 1 "HH:mm:ss,SSS" "13:45:49,119")
    <> (renderTime times 2 "mm:ss,SSS" "45:49,119")
    <> (renderTime times 3 "mm:ss,SSS" "45:49,119")
    <> (renderTime times 4 "mm:ss,SS" "45:49,11")
    <> (renderTime times 5 "mm:ss,S" "45:49,1")
    <> (renderTime times (-1) "HH:mm:m:ss:SS,Sa" "---")

    -- pure $ HH.h1_ [ HH.text "Date" ]
    -- renderDate 0 "YYYY:MM:DD" "2017:12:27"
    -- renderDate 1 "YYYY:MM" "2017:12"
    -- renderDate 2 "YYYY" "2017"

  renderTime ∷ Map TimeIdx String -> TimeIdx -> String -> String -> Array (HTML m)
  renderTime times idx fmtStr timeStr = unEither $ do
    fmt <- TimeF.fromString fmtStr
    time <- TimeF.unformat fmt timeStr
    pure
      [ HH.slot (TimeSlot idx) (Time.picker fmt) unit (HE.input (HandleTimeMessage idx))
      , HH.button [ HE.onClick $ HE.input_ $ SetTime idx time] [ HH.text $ "set: " <> timeStr ]
      , case lookup idx times of
          Nothing -> HH.div_ [HH.text "no time is set"]
          Just currentTime -> HH.div_ [HH.text $ "time:" <> currentTime]
      ]
    where
    unEither :: Either String (Array (HTML m)) -> Array (HTML m)
    unEither (Left err) = [HH.div_ [HH.text err]]
    unEither (Right a) = a


  eval ∷ ExampleQuery ~> H.ParentDSL State ExampleQuery Time.Query ExampleSlot Void m
  eval (SetTime idx time next) = do
    isSet <- H.query (TimeSlot idx) $ H.action $ left <<< (SetValue time)
    -- TODO check if isSet is Just
    -- Looks like it's `Maybe Unit`
    D.traceAnyA isSet
    pure next

  eval (HandleTimeMessage idx msg next) = do
    case msg of
      NotifyChange t -> H.modify \s -> s {times = insert idx (show t) s.times}

    pure next
