module Halogen.Datapicker.Component.Types where

import Data.Maybe (Maybe, fromJust)
import Data.Unit (Unit)
import Partial.Unsafe (unsafePartialBecause)

data PickerQuery e z a
  = GetValue (z -> a)
  | SetValue z (e -> a)

data PickerMessage z
  = NotifyChange z

mustBeMounted :: Maybe Unit -> Unit
mustBeMounted = unsafePartialBecause "children must be mounted" fromJust
