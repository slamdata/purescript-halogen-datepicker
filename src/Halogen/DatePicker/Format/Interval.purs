module Halogen.Datepicker.Format.Interval
  ( Format
  ) where

import Halogen.Datepicker.Format.Duration as DurationF
import Halogen.Datepicker.Format.DateTime as DateTimeF
import Data.Interval (Interval)

type Format = Interval DurationF.Format DateTimeF.Format
