module Halogen.Datepicker.Format.Interval where

import Data.Interval (Interval)
import Halogen.Datepicker.Format.DateTime as DateTimeF
import Halogen.Datepicker.Format.Duration as DurationF

type Format = Interval DurationF.Format DateTimeF.Format
