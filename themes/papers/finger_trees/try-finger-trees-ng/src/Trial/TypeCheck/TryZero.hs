module Trial.TypeCheck.TryZero where

import Control.Monad.Writer
import Data.Maybe

import Data.Derivation.Zero
import Trial.TypeCheck.ExpParser
import Data.Derivation.AvoidNegative
import Data.Derivation.Polynominal

samplePolynominal :: Writer [Zero String] (Polynominal String)
samplePolynominal = termToPolynominal . fst . fromJust . parseTerm $ tokens "((m + m) - 2)"
