{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Derivation.CanDerive where

import Outputable hiding (empty)
import Control.Arrow
import Data.Either
import Data.List
import Data.Map.Strict hiding ((\\), foldl, null, partition, take)

import Data.Derivation.Zero
import Data.Derivation.Expression
import Data.Derivation.AvoidNegative

import qualified Data.Derivation.Zero as Z

canDerive :: Ord v => Given v -> Wanted v -> Bool
canDerive g w =
	selfContained w ||
	elemBy isDerivableFrom (wantedToZero w) (givenToZeros $ removeVars g rv)
--	wantedToZero w `elem` givenToZeros (removeVars g rv)
	where rv = containVarsG g \\ containVarsW w

elemBy :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy eq = any . eq

newtype Wanted v = Wanted (Zero v) deriving Show

expToWanted :: Ord v => Exp v Bool -> (Maybe (Wanted v), [Wanted v])
expToWanted = ((Wanted <$>) *** (Wanted <$>)) . \e -> eqToZero' e True empty

wantedToZero :: Wanted v -> Zero v
wantedToZero (Wanted z) = z

containVarsW :: Ord v => Wanted v -> [Maybe v]
containVarsW = Z.containVars . wantedToZero

selfContained :: Wanted v -> Bool
selfContained (Wanted z) = identity z

instance Show v => Outputable (Wanted v) where
	ppr = text . show

debugWanted :: Wanted String
debugWanted = Wanted debugZeroWanted

newtype Given v = Given [Zero v] deriving Show

given :: Ord v => [Zero v] -> Given v
given zs = Given . nub . sort $ zs ++ take 8 (noNegativeFromG <$> zs)

expsToGiven :: Ord v => [Exp v Bool] -> Given v
expsToGiven es = given . concat $ (\e -> uncurry (maybe id (:)) $ eqToZero' e True vb) <$> es
	where vb = expToVarBool es

givenToZeros :: Given v -> [Zero v]
givenToZeros (Given zs) = zs

containVarsG :: Ord v => Given v -> [Maybe v]
containVarsG = nub . sort . concat . (Z.containVars <$>) . givenToZeros

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

removeVarG :: Ord v => Given v -> Maybe v -> Given v
removeVarG g v = Given . sort $ r ++ concat (fst $ unfoldUntil null (removeVarStep v) z)
	where (z, r) = removeVarInit g v

removeVars :: Ord v => Given v -> [Maybe v] -> Given v
removeVars = foldl removeVarG

instance Show v => Outputable (Given v) where
	ppr = text . show

debugGiven = Given debugZeros
