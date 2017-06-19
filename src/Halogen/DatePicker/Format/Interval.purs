module Halogen.Datapicker.Format.Interval
  ( Format
  ) where

import Halogen.Datapicker.Format.Duration as DurationF
import Halogen.Datapicker.Format.DateTime as DateTimeF
import Data.Interval (Interval)

type Format = Interval DurationF.Format DateTimeF.Format
