module Halogen.Datapicker.Component.Interval.Format
  ( Format
  ) where

import Halogen.Datapicker.Component.Duration.Format as DurationF
import Halogen.Datapicker.Component.DateTime.Format as DateTimeF
import Data.Interval (Interval)

type Format = Interval DurationF.Format DateTimeF.Format
