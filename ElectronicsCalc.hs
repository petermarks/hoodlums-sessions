{-# language TypeSynonymInstances, FlexibleInstances #-}

module ElectronicsCalc where

import Numeric.Units.Dimensional.TF.Prelude hiding (Power)
import qualified Prelude
import Text.Printf
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Token
import Text.Parsec.String
import Control.Applicative

type Resistance = ElectricResistance  Double
type Current    = ElectricCurrent     Double
type Voltage    = PotentialDifference Double
type Power      = Quantity DPower     Double

data ElecInfo = ElecInfo
  { resistance :: Resistance
  , current    :: Current
  , voltage    :: Voltage
  , power      :: Power
  }

test :: Resistance
test = (30 *~ volt) / (5 *~ ampere)

class Pretty a where
  pretty :: a -> String

instance Pretty Resistance where
  pretty x = printf "%f ohm" (x /~ ohm)

instance Pretty Current where
  pretty x = printf "%f A" (x /~ ampere)

instance Pretty Voltage where
  pretty x = printf "%f V" (x /~ volt)

instance Pretty Power where
  pretty x = printf "%f W" (x /~ watt)

instance Pretty ElecInfo where
  pretty (ElecInfo r i v p) = unlines [pretty r, pretty i, pretty v, pretty p]

resistanceP :: Parser Resistance
resistanceP = (*~ ohm) <$> float haskell <* char 'o'