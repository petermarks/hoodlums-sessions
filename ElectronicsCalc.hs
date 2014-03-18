module ElectronicsCalc where

import Numeric.Units.Dimensional.TF.Prelude
import qualified Prelude

test = (30 *~ volt) / (5 *~ ampere)

