module Halogen.Datapicker.Component.Types where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Text.Parsing.Parser (Parser, ParseError)
import Data.Functor.Coproduct (Coproduct)
import Data.Either (Either)
import Data.Tuple (Tuple)

data PickerQuery z a
  = GetValue (Tuple String z -> a)
  | SetValue (Either String z) a

data PickerMessage
  = NotifyChange
  | ParserFailed ParseError String

type PickerSpec query msg m a =
  { unformat ∷ Parser String a
  , format ∷ a -> String
  , component ∷ H.Component HH.HTML (query a) Unit msg m
  }

type Picker query msg m a = H.Component HH.HTML (Coproduct (PickerQuery a) query) Unit (Either PickerMessage msg) m

-- mkPicker ∷ ∀ query msg m a
--   . PickerSpec query msg m a
--   → Picker query msg m a
-- mkPicker = -- TODO we need somethink like overComponent to decorate it's `eval`
