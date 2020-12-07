module Trial.TypeCheck.TryZero where

import Control.Monad.Writer
import Data.Maybe

import Data.Derivation.Constraint
import Data.Derivation.Expression
import Data.Derivation.Parse

samplePolynomial :: Writer [Constraint String] (Polynomial String)
samplePolynomial = termToPolynomial . fromJust $ parse polynomial "((m + m) - 2)"
