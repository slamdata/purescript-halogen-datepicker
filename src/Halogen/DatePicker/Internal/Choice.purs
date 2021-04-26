module Halogen.Datepicker.Internal.Choice where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Array (cons)
import Data.Enum (class BoundedEnum, fromEnum, toEnum)
import Data.Foldable (elem, for_)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.NonEmpty (NonEmpty, fromNonEmpty, head, tail)
import Data.Number as N
import Effect.Exception as Ex
import Halogen as H
import Halogen.Datepicker.Component.Types (BasePickerQuery(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM (HalogenM)

type State val = {value ∷ val}

type Query val = BasePickerQuery (Maybe ChoiceError) val
data ChoiceError = ValueIsNotInValues

type Slots ∷ ∀ k. Row k
type Slots = ()

type Slot val = H.Slot (Query val) val

type DSL val = H.HalogenM (State val) (Maybe val) Slots val
type HTML val m = H.ComponentHTML (Maybe val) Slots m

type Config val =
  { title ∷ String
  , values ∷ NonEmpty Array val
  , root ∷ Array HH.ClassName
  }

picker
  ∷ ∀ val m
  . Ord val
  ⇒ HasChoiceInputVal val
  → Config val
  → H.Component (Query val) Unit val m
picker hasChoiceInputVal config =
  H.mkComponent
    { initialState: const { value: head config.values }
    , render: render config hasChoiceInputVal
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery config.values
        }
    }

render
  ∷ ∀ val m
  . Eq val
  ⇒ Config val
  → HasChoiceInputVal val
  → State val
  → HTML val m
render config hasChoiceInputVal {value} =
  HH.select
    [ HP.title config.title
    , HP.classes config.root
    , HE.onValueChange hasChoiceInputVal.fromString
    ] (fromNonEmpty cons config.values <#> renderValue)
  where
  renderValue value' = HH.option
    [ HP.value $ hasChoiceInputVal.toValue value'
    , HP.selected (value' == value)
    ]
    [ HH.text $ hasChoiceInputVal.toTitle value' ]

handleAction ∷ ∀ val m. Eq val ⇒ Maybe val → DSL val m Unit
handleAction value = do
  s ← H.get
  -- there wouldn't be case when value is Nothing so it's fine to do `for_`
  for_ value \value' → do
    H.modify_ _{value = value'}
    when (value' /= s.value) $ H.raise value'

handleQuery
  ∷ ∀ val m a
  . Eq val
  ⇒ NonEmpty Array val
  → Query val a
  → DSL val m (Maybe a)
handleQuery values = case _ of
  SetValue value k
    | value == head values || elem value (tail values) → do
        H.modify_ _{value = value}
        pure $ Just $ k Nothing
    | otherwise →
        pure $ Just $ k (Just ValueIsNotInValues)
  GetValue k →
    Just <<< k <$> H.gets _.value

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

boundedEnumHasChoiceInputVal
  ∷ ∀ a
  . BoundedEnum a
  ⇒ (a → String)
  → HasChoiceInputVal a
boundedEnumHasChoiceInputVal showTitle =
  { fromString: intHasChoiceInputVal.fromString >=> toEnum
  , toValue: fromEnum >>> intHasChoiceInputVal.toValue
  , toTitle: showTitle
  }

maybeIntHasChoiceInputVal
  ∷ (Maybe Int → String)
  → HasChoiceInputVal (Maybe Int)
maybeIntHasChoiceInputVal showTitle =
  { fromString: \str → if str == ""
      then pure Nothing
      else intHasChoiceInputVal.fromString str <#> pure
  , toValue: maybe "" show
  , toTitle: showTitle
  }

maybeBoundedEnumHasChoiceInputVal
  ∷ ∀ a
  . BoundedEnum a
  ⇒ (a → String)
  → HasChoiceInputVal (Maybe a)
maybeBoundedEnumHasChoiceInputVal showTitle =
  { fromString: \str → if str == ""
      then pure Nothing
      else intHasChoiceInputVal.fromString str <#> toEnum
  , toValue: maybe "" (show <<< fromEnum)
  , toTitle: maybe "" showTitle
  }

valueMustBeInValues
  ∷ ∀ s f ps o m
  . MonadError Ex.Error m
  ⇒ Maybe ChoiceError
  → HalogenM s f ps o m Unit
valueMustBeInValues = case _ of
  Just ValueIsNotInValues →
    throwError $ Ex.error "Value being set in Choice is not in values"
  Nothing →
    pure unit
