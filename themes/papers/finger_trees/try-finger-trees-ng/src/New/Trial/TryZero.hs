module New.Trial.TryZero where

import Data.Maybe

import New.Polynominal.Zero
import New.Trial.ExpParser
import New.Polynominal
import New.Polynominal.Type

samplePolynominal :: Polynominal String
samplePolynominal = termToPolynominal . fst . fromJust . parseTerm $ tokens "((m + m) - 2)"
