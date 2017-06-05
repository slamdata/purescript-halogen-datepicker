module Halogen.Datapicker.Component.Duration where

import Prelude
import Debug.Trace as D
import Halogen as H
import Halogen.Datapicker.Component.Duration.Format as F
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Control.MonadPlus (guard)
import Data.Either (Either(Right, Left), either)
import Data.Foldable (foldMap, foldl)
import Data.Functor.Coproduct (Coproduct, coproduct, right)
import Data.Interval (Duration, IsoDuration, mkIsoDuration, unIsoDuration)
import Data.List (List)
import Data.Map (Map, empty, insert, isEmpty, lookup, member, toUnfoldable)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (unwrap)
import Data.Number (fromString)
import Data.String (Pattern(..), stripSuffix)
import Data.Tuple (Tuple(..))
import Halogen.Datapicker.Component.Internal.Elements (numberElement, minRange)
import Halogen.Datapicker.Component.Types (PickerQuery(..), PickerMessage(..))


data DurationQuery a = UpdateCommand F.Command String a

type Query = Coproduct (PickerQuery Unit IsoDuration) DurationQuery
type Message = PickerMessage IsoDuration

type TempChange = Either Number String
type TempChanges = Map F.Command TempChange
type State =
  { format :: F.Format
  , duration :: IsoDuration
  , temp :: TempChanges
  }


type DSL = H.ComponentDSL State Query Message
type HTML = H.ComponentHTML DurationQuery



picker ∷ ∀ m. F.Format -> IsoDuration -> H.Component HH.HTML Query Unit Message m
picker format duration = H.component
  { initialState: const {format, duration, temp: empty}
  , render: render <#> (map right)
  , eval: coproduct evalPicker evalDuration
  , receiver: const Nothing
  }

render ∷ State -> HTML
render s =
  HH.ul
    [ HP.classes $
      [ HH.ClassName "Picker" ]
      <> (guard invalids.form $> HH.ClassName "Picker--invalid")
    ]
    (foldMap (pure <<< f) (unwrap s.format))
  where
  invalids :: {form :: Boolean, commands :: Map F.Command Unit}
  invalids = case applyTemp s.temp s.duration of
    Left commands -> {form : isEmpty commands, commands}
    Right       _ -> {form : false, commands: empty}
  f cmd = HH.li
    [ HP.classes [ HH.ClassName "Picker-component" ] ]
    [ renderCommand
      numStr
      (member cmd invalids.commands)
      cmd ]
    where
    invalidNumber = lookup cmd s.temp >>= either Just (const Nothing)
    numStr = showNum $ maybe (getComponent cmd s.duration) id invalidNumber

renderCommand :: String -> Boolean -> F.Command -> HTML
renderCommand num isInvalid cmd =
  numberElement
    (UpdateCommand cmd)
    { title: show cmd
    , range: minRange 0.0
    , invalid: isInvalid
    }
    num

showNum :: Number -> String
showNum 0.0 = "0"
showNum n = let str = show n
  in maybe str id (stripSuffix (Pattern ".0") str)

getComponent :: F.Command -> IsoDuration -> Number
getComponent cmd d = maybe 0.0 id $ F.toGetter cmd (unIsoDuration d)

applyChange :: F.Command -> Number -> Duration -> Duration
applyChange cmd val dur = F.toSetter cmd val dur

type PartitionStep = Tuple (Map F.Command Unit) (Endo Duration)

applyTemp
  ∷ TempChanges
  → IsoDuration
  → Either (Map F.Command Unit) IsoDuration
applyTemp temp duration = case parts of
  Tuple invalids (Endo durFunc) -> if isEmpty invalids
    then maybe (Left invalids) Right (mkIsoDuration $ durFunc $ unIsoDuration duration)
    else Left invalids
  where
  parts :: PartitionStep
  parts = foldl partition mempty (toUnfoldable temp :: List (Tuple F.Command TempChange))
  partition ::  PartitionStep -> Tuple F.Command TempChange -> PartitionStep
  partition (Tuple invalids durEndo) (Tuple cmd change) = case change of
    Left num -> Tuple invalids (Endo (applyChange cmd num) <> durEndo)
    Right  _ -> Tuple (insert cmd unit invalids) durEndo


evalDuration ∷ ∀ m . DurationQuery ~> DSL m
evalDuration (UpdateCommand cmd valStr next) = do
  ({ duration, temp }) <- H.get
  let valEither = maybe (Right valStr) Left (fromString valStr)
  let temp' = insert cmd valEither temp
  case applyTemp temp' duration of
    Left _ -> H.modify _{temp = temp'}
    Right nextDuration -> do
      H.modify _{duration = nextDuration, temp = empty :: TempChanges}
      when (nextDuration /= duration) $ H.raise (NotifyChange nextDuration)
  pure next

evalPicker ∷ ∀ m . (PickerQuery Unit IsoDuration) ~> DSL m
evalPicker (SetValue duration next) = do
  H.modify _{ duration = duration }
  pure $ next unit
evalPicker (GetValue next) = H.gets _.duration <#> next
