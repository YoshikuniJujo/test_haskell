{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module New.Polynominal.Given where

import Control.Arrow
import Data.Either
import Data.List

import New.Expression
import New.Polynominal
import New.Polynominal.Zero hiding (removeVar)
import qualified New.Polynominal.Zero as Z

newtype Given v = Given [Zero v] deriving Show

given :: Ord v => [Zero v] -> Given v
given = Given . nub . sort

expsToGiven :: Ord v => [Exp v Bool] -> Maybe (Given v)
expsToGiven es = do
	given <$> (\e -> eqToZero e True vb) `mapM` es
	where vb = expToVarBool es

givenToZeros :: Given v -> [Zero v]
givenToZeros (Given zs) = zs

removeVarInit :: Ord v => Given v -> v -> ([Zero v], [Zero v])
removeVarInit (Given zs) v = partition (`doesContainVar` v) zs

removeVar1 :: Ord v => Zero v -> v -> Zero v -> Either (Zero v) (Zero v)
removeVar1 z0 v z = case Z.removeVar z0 z v of
	Just z' -> Left z'; Nothing -> Right z

removeVarStep :: Ord v => v -> [Zero v] -> ([Zero v], [Zero v])
removeVarStep _ [] = ([], [])
removeVarStep v (z : zs) = partitionEithers $ removeVar1 z v <$> zs

unfoldUntil :: (s -> Bool) -> (s -> (r, s)) -> s -> ([r], s)
unfoldUntil p f s0
	| p s0 = ([], s0)
	| otherwise = let (r, s') = f s0 in (r :) `first` unfoldUntil p f s'

removeVar :: Ord v => Given v -> v -> Given v
removeVar g v = Given $ r ++ concat (fst $ unfoldUntil null (removeVarStep v) z)
	where (z, r) = removeVarInit g v

removeVars :: Ord v => Given v -> [v] -> Given v
removeVars = foldl removeVar
