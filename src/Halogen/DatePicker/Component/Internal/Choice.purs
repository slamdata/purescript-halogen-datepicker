module Halogen.Datapicker.Component.Internal.Choice
  ( picker
  , ChoiceQuery
  , Query
  , QueryIn
  , ChoiceError
  )
  where

import Prelude
import Data.Int as Int
import Data.Number as N
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.Array (cons)
import Data.Enum (class BoundedEnum, fromEnum, toEnum)
import Data.Foldable (elem, for_)
import Data.Functor.Coproduct (Coproduct, coproduct, right)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, fromNonEmpty, head, tail)
import Halogen.Datapicker.Component.Types (PickerMessage(..), PickerQuery(..))



type State val =
  { value:: val
  , values:: NonEmpty Array val
  , title:: String
  }

data ChoiceError = ValueIsNotInValues

data ChoiceQuery val a = Update (Maybe val) a
type QueryIn val = PickerQuery (Maybe ChoiceError) val
type Query val = Coproduct (QueryIn val) (ChoiceQuery val)
type Message val = PickerMessage val

type DSL val = H.ComponentDSL (State val) (Query val) (Message val)
type HTML val = H.ComponentHTML (ChoiceQuery val)

picker ∷ ∀ val m
  . Ord val
  => HasChoiceInputVal val
  -> {title :: String, values :: NonEmpty Array val}
  -> H.Component HH.HTML (Query val) Unit (Message val) m
picker hasChoiceInputVal {title, values} = H.component
  { initialState: const {title, values, value: head values}
  , render: (render hasChoiceInputVal) <#> (map right)
  , eval: coproduct (evalPicker hasChoiceInputVal) evalChoice
  , receiver: const Nothing
  }

render ∷ ∀ val. Eq val => HasChoiceInputVal val -> State val -> HTML val
render hasChoiceInputVal {title, values, value}  = HH.select
  [ HP.title title
  , HP.classes classes
  , HE.onValueChange (HE.input (hasChoiceInputVal.fromString >>> Update))
  ] (fromNonEmpty cons values <#> renderValue)
  where
  renderValue value' = HH.option
    [ HP.value $ hasChoiceInputVal.toValue value'
    , HP.selected (value' == value)
    ]
    [ HH.text $ hasChoiceInputVal.toTitle value ]
  classes = [HH.ClassName "Picker-input"]
    -- <> (guard (isInvalid value) $> HH.ClassName "Picker-input--invalid")


evalChoice ∷ ∀ val m . Eq val => ChoiceQuery val ~> DSL val m
evalChoice (Update value next) = do
  s <- H.get
  for_ value \value' -> do
    H.modify _{value = value'}
    when (value' /= s.value) $ H.raise (NotifyChange $ s.value)
  pure next


evalPicker ∷ ∀ val m . Eq val => HasChoiceInputVal val -> QueryIn val ~> DSL val m
evalPicker hasChoiceInputVal (SetValue value next) = do
  {values} <- H.get
  if (value == head values || elem value (tail values))
    then do
      H.modify _{value = value}
      pure $ next Nothing
    else do
      pure $ next (Just ValueIsNotInValues)
evalPicker _ (GetValue next) = H.gets _.value <#> next



type HasChoiceInputVal a =
  { fromString :: String -> Maybe a
  , toValue :: a -> String
  , toTitle :: a -> String
  -- TODO add isValid
  }

stringHasChoiceInputVal :: HasChoiceInputVal String
stringHasChoiceInputVal =
  { fromString: pure
  , toValue: id
  , toTitle: id
  }
numberHasChoiceInputVal :: HasChoiceInputVal Number
numberHasChoiceInputVal =
  { fromString: N.fromString
  , toValue: show
  , toTitle: show
  }

intHasChoiceInputVal :: HasChoiceInputVal Int
intHasChoiceInputVal =
  { fromString: Int.fromString
  , toValue: show
  , toTitle: show
  }

boundedEnumHasChoiceInputVal :: ∀ a. Show a => BoundedEnum a => HasChoiceInputVal a
boundedEnumHasChoiceInputVal =
  { fromString: intHasChoiceInputVal.fromString >=> toEnum
  , toValue: fromEnum >>> intHasChoiceInputVal.toValue
  , toTitle: show
  }

-- TODO implement
-- maybeBoundedEnumHasChoiceInputVal :: ∀ a. Show a => BoundedEnum a => HasChoiceInputVal (Maybe a)
