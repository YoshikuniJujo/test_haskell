{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Derivation.CanDerive where

import Control.Arrow (first)
import Data.Either
import Data.List ((\\), nub, partition, sort)
import Data.Map.Strict (empty)

import Data.Derivation.Constraint (
	Constraint, getVars, hasVar,
	rmNegative, isDerivFrom, selfContained )
import Data.Derivation.Expression

import qualified Data.Derivation.Constraint as C
