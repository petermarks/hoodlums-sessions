{-# language TypeSynonymInstances, FlexibleInstances #-}

module ElectronicsCalc where

import Numeric.Units.Dimensional.TF.Prelude hiding (Power)
import qualified Prelude
import Text.Printf

type Resistance = ElectricResistance  Rational
type Current    = ElectricCurrent     Rational
type Voltage    = PotentialDifference Rational
type Power      = Quantity DPower     Rational

test :: Resistance
test = (30 *~ volt) / (5 *~ ampere)

class Pretty a where
  pretty :: a -> String

instance Pretty Resistance where
  pretty x = printf "%f ohm" (fromRational $ x /~ ohm :: Double)

instance Pretty Current where
  pretty x = printf "%f A" (fromRational $ x /~ ampere :: Double)

instance Pretty Voltage where
  pretty x = printf "%f V" (fromRational $ x /~ volt :: Double)

instance Pretty Power where
  pretty x = printf "%f W" (fromRational $ x /~ watt :: Double)

