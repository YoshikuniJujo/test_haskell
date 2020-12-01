module Trial.TypeCheck.TryZero where

import Data.Maybe

import Polynominal.Zero
import Trial.TypeCheck.ExpParser
import Polynominal
import Polynominal.Type

samplePolynominal :: Polynominal String
samplePolynominal = termToPolynominal . fst . fromJust . parseTerm $ tokens "((m + m) - 2)"
