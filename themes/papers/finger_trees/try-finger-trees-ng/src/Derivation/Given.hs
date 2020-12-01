{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Derivation.Given where

import Outputable

import Control.Arrow
import Data.Maybe
import Data.Either
import Data.List

import Derivation.Expression
import Derivation.AvoidNegative
import Derivation.Zero hiding (removeVar, containVars)
import qualified Derivation.Zero as Z

newtype Given v = Given [Zero v] deriving Show

given :: Ord v => [Zero v] -> Given v
given zs = Given . nub . sort $ zs ++ take 8 (noNegativeFromG <$> zs)

expsToGiven :: Ord v => [Exp v Bool] -> Given v
expsToGiven es = given . concat $ (\e -> uncurry (maybe id (:)) $ eqToZero' e True vb) <$> es
	where vb = expToVarBool es

givenToZeros :: Given v -> [Zero v]
givenToZeros (Given zs) = zs

containVars :: Ord v => Given v -> [Maybe v]
containVars = nub . sort . concat . (Z.containVars <$>) . givenToZeros

removeVarInit :: Ord v => Given v -> Maybe v -> ([Zero v], [Zero v])
removeVarInit (Given zs) v = partition (`doesContainVar` v) zs

removeVar1 :: Ord v => Zero v -> Maybe v -> Zero v -> Either (Zero v) (Zero v)
removeVar1 z0 v z = case Z.removeVar z0 z v of
	Just z' -> Left z'; Nothing -> Right z

removeVarStep :: Ord v => Maybe v -> [Zero v] -> ([Zero v], [Zero v])
removeVarStep _ [] = ([], [])
removeVarStep v (z : zs) = partitionEithers $ removeVar1 z v <$> zs

unfoldUntil :: (s -> Bool) -> (s -> (r, s)) -> s -> ([r], s)
unfoldUntil p f s0
	| p s0 = ([], s0)
	| otherwise = let (r, s') = f s0 in (r :) `first` unfoldUntil p f s'

removeVar :: Ord v => Given v -> Maybe v -> Given v
removeVar g v = Given . sort $ r ++ concat (fst $ unfoldUntil null (removeVarStep v) z)
	where (z, r) = removeVarInit g v

removeVars :: Ord v => Given v -> [Maybe v] -> Given v
removeVars = foldl removeVar

instance Show v => Outputable (Given v) where
	ppr = text . show

debugGiven = Given debugZeros
