module Halogen.Datepicker.Internal.Utils where

import Prelude

import Control.Alternative (class Alternative, empty)
import Control.Monad.Error.Class (class MonadError, throwError)
import Control.MonadPlus (guard)
import Data.Bifunctor (bimap, lmap)
import Data.Either (Either(..), either)
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect.Exception as Ex
import Halogen as H
import Halogen.Datepicker.Component.Types (PickerMessage(..), PickerValue, isInvalid)
import Halogen.Datepicker.Config (Config(..))
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM (HalogenM')

mustBeMounted
  ∷ ∀ s f ps o m a
  . MonadError Ex.Error m
  ⇒ Maybe a
  → HalogenM' s f ps o m a
mustBeMounted (Just x) = pure x
mustBeMounted _ = throwError $ Ex.error "children must be mounted"

pickerProps
  ∷ ∀ e a r z
  . Config
  → PickerValue e a
  → Array (HP.IProp ( "class" ∷ String | z ) r )
pickerProps (Config {root, rootInvalid}) val = [HP.classes classes]
  where
  classes = root <> (guard (isInvalid val) *> rootInvalid)

componentProps
  ∷ ∀ r z
  . Config
  → Array (HP.IProp ( "class" ∷ String | z ) r )
componentProps (Config {component})= [HP.classes component]

asRight ∷ ∀ e a f. Alternative f ⇒ Either e a → f a
asRight = either (const empty) pure

asLeft ∷ ∀ e a f. Alternative f ⇒ Either e a → f e
asLeft = either pure (const empty)

transitionState'
  ∷ ∀ f ps m val err
  . Eq err
  ⇒ Eq val
  ⇒ err
  → ( PickerValue err val
    → TransitionM f ps m err val (Either Boolean val)
    )
  → TransitionM f ps m err val Unit
transitionState' err f = transitionState (f >>>  (map $ lmap (_ `Tuple` err)))

type TransitionM f ps m err val =
  HalogenM' (PickerValue err val) f ps (PickerMessage (PickerValue err val)) m

transitionState
  ∷ ∀ f ps m val err
  . Eq err
  ⇒ Eq val
  ⇒ ( PickerValue err val
    → TransitionM f ps m err val (Either (Tuple Boolean err) val)
    )
  → TransitionM f ps m err val Unit
transitionState f = do
  val ← H.get
  nextVal ← map (steper val) (f val)
  val `moveStateTo` nextVal
  where
  moveStateTo ∷ ∀ a. Eq a ⇒ a → a → HalogenM' a f ps (PickerMessage a) m Unit
  moveStateTo old new = H.put new *> unless (new == old) (H.raise $ NotifyChange new)
  steper ∷ ∀ e a. PickerValue e a → Either (Tuple Boolean e) a → PickerValue e a
  steper old new = case old, new of
    _, Right x → Just (Right x)
    Just _, Left (Tuple _ err) → Just (Left err)
    -- `true` indicates if we want to force state change to "invalid"
    Nothing, Left (Tuple true err) → Just (Left err)
    Nothing, Left _ → Nothing

foldSteps ∷ ∀ a. Monoid a ⇒ Array (Maybe a) → Maybe a
foldSteps steps = map fold $ sequence steps

mapComponentHTMLQuery
  ∷ ∀ f f' ps m
  . (f ~> f')
  → H.ComponentHTML f ps m
  → H.ComponentHTML f' ps m
mapComponentHTMLQuery f = bimap (map f) f
