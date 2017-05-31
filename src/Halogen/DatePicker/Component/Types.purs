module Halogen.Datapicker.Component.Types where

data PickerQuery z a
  = GetValue (z -> a)
  | SetValue z a

data PickerMessage z
  = NotifyChange z
