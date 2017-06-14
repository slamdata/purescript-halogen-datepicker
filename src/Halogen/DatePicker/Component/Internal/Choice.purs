module Halogen.Datapicker.Component.Internal.Choice
  ( picker
  , ChoiceQuery
  , Query
  , QueryIn
  , ChoiceError(..)
  , HasChoiceInputVal
  , stringHasChoiceInputVal
  , numberHasChoiceInputVal
  , intHasChoiceInputVal
  , boundedEnumHasChoiceInputVal
  , maybeIntHasChoiceInputVal
  , maybeBoundedEnumHasChoiceInputVal
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
import Halogen.Datapicker.Component.Types (PickerMessage(..), BasePickerQuery(..))



type State val =
  { value:: val
  , values:: NonEmpty Array val
  , title:: String
  }

data ChoiceError = ValueIsNotInValues

data ChoiceQuery val a = Update (Maybe val) a
type QueryIn val = BasePickerQuery (Maybe ChoiceError) val
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
  { initialState: const {title, values, value: head $ values}
  , render: (render hasChoiceInputVal) <#> (map right)
  , eval: coproduct (evalPicker hasChoiceInputVal) evalChoice
  , receiver: const Nothing
  }

render ∷ ∀ val. Eq val => HasChoiceInputVal val -> State val -> HTML val
render hasChoiceInputVal {title, values, value}  = HH.select
  [ HP.title title
  , HP.classes [HH.ClassName "Picker-input"]
  , HE.onValueChange (HE.input (hasChoiceInputVal.fromString >>> Update))
  ] (fromNonEmpty cons values <#> renderValue)
  where
  renderValue value' = HH.option
    [ HP.value $ hasChoiceInputVal.toValue value'
    , HP.selected (value' == value)
    ]
    [ HH.text $ hasChoiceInputVal.toTitle value' ]


evalChoice ∷ ∀ val m . Eq val => ChoiceQuery val ~> DSL val m
evalChoice (Update value next) = do
  s <- H.get
  -- there wouldn't be case when value is Nothing so it's fine to do `for_`
  for_ value \value' -> do
    H.modify _{value = value'}
    when (value' /= s.value) $ H.raise (NotifyChange $ value')
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

boundedEnumHasChoiceInputVal :: ∀ a. BoundedEnum a => (a -> String) -> HasChoiceInputVal a
boundedEnumHasChoiceInputVal showTitle =
  { fromString: intHasChoiceInputVal.fromString >=> toEnum
  , toValue: fromEnum >>> intHasChoiceInputVal.toValue
  , toTitle: showTitle
  }

maybeIntHasChoiceInputVal :: (Int -> String) -> HasChoiceInputVal (Maybe Int)
maybeIntHasChoiceInputVal showTitle =
  { fromString: \str -> if str == ""
      then pure Nothing
      else intHasChoiceInputVal.fromString str <#> pure
  , toValue: case _ of
      Nothing -> ""
      Just x -> show x
  , toTitle: case _ of
      Nothing -> ""
      Just x -> showTitle x
  }

maybeBoundedEnumHasChoiceInputVal :: ∀ a. BoundedEnum a => (a -> String) -> HasChoiceInputVal (Maybe a)
maybeBoundedEnumHasChoiceInputVal showTitle =
  { fromString: \str -> if str == ""
      then pure Nothing
      else intHasChoiceInputVal.fromString str <#> toEnum
  , toValue: case _ of
      Nothing -> show $ ""
      Just x -> show $ fromEnum x
  , toTitle: case _ of
      Nothing -> ""
      Just x -> showTitle x
  }
