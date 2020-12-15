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

newtype Given v = Given { unGiven :: [Constraint v] } deriving Show

mkGiven :: Ord v => [Exp v Bool] -> Given v
mkGiven es = given . concat
	$ uncurry (maybe id (:)) . mkConstraint (mkVarBool es) <$> es

given :: Ord v => [Constraint v] -> Given v
given zs = Given . nub . sort $ zs ++ (rmNegative <$> zs)

givenVars :: Ord v => Given v -> [Maybe v]
givenVars = nub . sort . concat . (getVars <$>) . unGiven
