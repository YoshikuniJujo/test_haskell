module Trial.TypeCheck.TryZero where

import Control.Monad.Writer
import Data.Maybe

import Derivation.Zero
import Trial.TypeCheck.ExpParser
import Derivation.AvoidNegative
import Derivation.Polynominal

samplePolynominal :: Writer [Zero String] (Polynominal String)
samplePolynominal = termToPolynominal . fst . fromJust . parseTerm $ tokens "((m + m) - 2)"
