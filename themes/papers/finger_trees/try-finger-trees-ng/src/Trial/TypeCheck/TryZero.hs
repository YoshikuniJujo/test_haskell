module Trial.TypeCheck.TryZero where

import Control.Monad.Writer
import Data.Maybe

import Data.Derivation.Constraint
import Data.Derivation.Expression
import Data.Derivation.Parse

samplePolynominal :: Writer [Constraint String] (Polynominal String)
samplePolynominal = termToPolynominal . fromJust $ parse polynomial "((m + m) - 2)"
