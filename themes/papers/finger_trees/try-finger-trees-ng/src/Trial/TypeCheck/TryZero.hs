module Trial.TypeCheck.TryZero where

import Control.Monad.Writer
import Data.Maybe

import Data.Derivation.Zero
import Data.Derivation.Expression
import Data.Derivation.Parse

samplePolynominal :: Writer [Zero String] (Polynominal String)
samplePolynominal = termToPolynominal . fromJust $ parse polynomial "((m + m) - 2)"
