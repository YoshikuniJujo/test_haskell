module Trial.TypeCheck.TryZero where

import Control.Monad.Writer
import Data.Maybe

import Derivation.Polynominal.Zero
import Trial.TypeCheck.ExpParser
import Derivation.Polynominal.AvoidNegative
import Derivation.Polynominal.Type

samplePolynominal :: Writer [Zero String] (Polynominal String)
samplePolynominal = termToPolynominal . fst . fromJust . parseTerm $ tokens "((m + m) - 2)"
