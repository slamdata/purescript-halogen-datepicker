module Halogen.Datepicker.Internal.Choice
  ( picker
  , ChoiceQuery
  , Query
  , QueryIn
  , Config
  , ChoiceError(..)
  , HasChoiceInputVal
  , stringHasChoiceInputVal
  , numberHasChoiceInputVal
  , intHasChoiceInputVal
  , boundedEnumHasChoiceInputVal
  , maybeIntHasChoiceInputVal
  , maybeBoundedEnumHasChoiceInputVal
  , valueMustBeInValues
  )
  where

import Prelude

import Data.Array (cons)
import Data.Enum (class BoundedEnum, fromEnum, toEnum)
import Data.Foldable (elem, for_)
import Data.Functor.Coproduct (Coproduct, coproduct, right)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.NonEmpty (NonEmpty, fromNonEmpty, head, tail)
import Data.Number as N
import Halogen as H
import Halogen.Datepicker.Component.Types (PickerMessage(..), BasePickerQuery(..))
import Halogen.Datepicker.Internal.Utils (mapComponentHTMLQuery)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM (HalogenM, halt)


type State val = {value ∷ val}

type Message val = PickerMessage val

type Query val = Coproduct (QueryIn val) (ChoiceQuery val)
type QueryIn val = BasePickerQuery (Maybe ChoiceError) val
data ChoiceError = ValueIsNotInValues
data ChoiceQuery val a = Update (Maybe val) a

type DSL val = H.ComponentDSL (State val) (Query val) (Message val)
type HTML val = H.ComponentHTML (ChoiceQuery val)

type Config val =
  { title ∷ String
  , values ∷ NonEmpty Array val
  , root ∷ Array HH.ClassName
  }

picker ∷ ∀ val m
  . Ord val
  ⇒ HasChoiceInputVal val
  → Config val
  → H.Component HH.HTML (Query val) Unit (Message val) m
picker hasChoiceInputVal config = H.component
  { initialState: const { value: head $ config.values }
  , render: render config hasChoiceInputVal >>> mapComponentHTMLQuery right
  , eval: coproduct (evalPicker config hasChoiceInputVal) evalChoice
  , receiver: const Nothing
  }

render ∷ ∀ val. Eq val ⇒ Config val → HasChoiceInputVal val → State val → HTML val
render config hasChoiceInputVal {value}  = HH.select
  [ HP.title config.title
  , HP.classes config.root
  , HE.onValueChange (HE.input (hasChoiceInputVal.fromString >>> Update))
  ] (fromNonEmpty cons config.values <#> renderValue)
  where
  renderValue value' = HH.option
    [ HP.value $ hasChoiceInputVal.toValue value'
    , HP.selected (value' == value)
    ]
    [ HH.text $ hasChoiceInputVal.toTitle value' ]


evalChoice ∷ ∀ val m . Eq val ⇒ ChoiceQuery val ~> DSL val m
evalChoice (Update value next) = do
  s ← H.get
  -- there wouldn't be case when value is Nothing so it's fine to do `for_`
  for_ value \value' → do
    H.modify_ _{value = value'}
    when (value' /= s.value) $ H.raise (NotifyChange $ value')
  pure next


evalPicker ∷ ∀ val m . Eq val ⇒ Config val → HasChoiceInputVal val → QueryIn val ~> DSL val m
evalPicker {values} hasChoiceInputVal (SetValue value next) = do
  if (value == head values || elem value (tail values))
    then do
      H.modify_ _{value = value}
      pure $ next Nothing
    else do
      pure $ next (Just ValueIsNotInValues)
evalPicker _ _ (GetValue next) = H.gets _.value <#> next


type HasChoiceInputVal a =
  { fromString ∷ String → Maybe a
  , toValue ∷ a → String
  , toTitle ∷ a → String
  }

stringHasChoiceInputVal ∷ HasChoiceInputVal String
stringHasChoiceInputVal =
  { fromString: pure
  , toValue: identity
  , toTitle: identity
  }
numberHasChoiceInputVal ∷ HasChoiceInputVal Number
numberHasChoiceInputVal =
  { fromString: N.fromString
  , toValue: show
  , toTitle: show
  }

intHasChoiceInputVal ∷ HasChoiceInputVal Int
intHasChoiceInputVal =
  { fromString: Int.fromString
  , toValue: show
  , toTitle: show
  }

boundedEnumHasChoiceInputVal ∷ ∀ a. BoundedEnum a ⇒ (a → String) → HasChoiceInputVal a
boundedEnumHasChoiceInputVal showTitle =
  { fromString: intHasChoiceInputVal.fromString >=> toEnum
  , toValue: fromEnum >>> intHasChoiceInputVal.toValue
  , toTitle: showTitle
  }

maybeIntHasChoiceInputVal ∷ (Maybe Int → String) → HasChoiceInputVal (Maybe Int)
maybeIntHasChoiceInputVal showTitle =
  { fromString: \str → if str == ""
      then pure Nothing
      else intHasChoiceInputVal.fromString str <#> pure
  , toValue: maybe "" show
  , toTitle: showTitle
  }

maybeBoundedEnumHasChoiceInputVal ∷ ∀ a. BoundedEnum a ⇒ (a → String) → HasChoiceInputVal (Maybe a)
maybeBoundedEnumHasChoiceInputVal showTitle =
  { fromString: \str → if str == ""
      then pure Nothing
      else intHasChoiceInputVal.fromString str <#> toEnum
  , toValue: maybe "" (show <<< fromEnum)
  , toTitle: maybe "" showTitle
  }

valueMustBeInValues :: ∀ s f g p o m. Maybe ChoiceError → HalogenM s f g p o m Unit
valueMustBeInValues = case _ of
  Just ValueIsNotInValues →
    halt "Value being set in Choice is not in values"
  Nothing →
    pure unit
