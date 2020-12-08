{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Derivation.CanDerive where

import Outputable hiding (empty)
import Control.Arrow
import Data.Either
import Data.List
import Data.Map.Strict hiding ((\\), foldl, null, partition, take)

import Data.Derivation.Constraint
import Data.Derivation.Expression

import qualified Data.Derivation.Constraint as Z

canDerive :: Ord v => Given v -> Wanted v -> Bool
canDerive g (Wanted ws) = canDeriveAll g ws

canDeriveMaybe :: Ord v => Given v -> Maybe (Wanted1 v) -> Bool
canDeriveMaybe g mw = maybe False (canDeriveGen g) mw

canDeriveAll :: Ord v => Given v -> [Wanted1 v] -> Bool
canDeriveAll g ws = all (canDeriveGen g) ws

canDeriveGen :: Ord v => Given v -> Wanted1 v -> Bool
canDeriveGen g w =
	selfContained w ||
	elemBy isDerivableFrom (wantedToZero w) (givenToZeros $ removeVars g rv)
	where rv = containVarsG g \\ containVarsW w

elemBy :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy eq = any . eq

newtype Wanted v = Wanted [Wanted1 v] deriving Show

wanted :: Maybe (Wanted1 v) -> [Wanted1 v] -> Maybe (Wanted v)
wanted mw ws = Wanted . (: ws) <$> mw

type Wanted1 v = Constraint v

expToWanted :: Ord v => Exp v Bool -> Maybe (Wanted v)
expToWanted = uncurry wanted . makeConstraint empty

wantedToZero :: Wanted1 v -> Constraint v
wantedToZero z = z

containVarsW :: Ord v => Wanted1 v -> [Maybe v]
containVarsW = Z.getVars . wantedToZero

instance Show v => Outputable (Constraint v) where
	ppr = text . show

newtype Given v = Given [Constraint v] deriving Show

given :: Ord v => [Constraint v] -> Given v
given zs = Given . nub . sort $ zs ++ (removeNegative <$> zs)

expsToGiven :: Ord v => [Exp v Bool] -> Given v
expsToGiven es = given . concat $ (\e -> uncurry (maybe id (:)) $ makeConstraint vb e) <$> es
	where vb = makeVarBool es

givenToZeros :: Given v -> [Constraint v]
givenToZeros (Given zs) = zs

containVarsG :: Ord v => Given v -> [Maybe v]
containVarsG = nub . sort . concat . (Z.getVars <$>) . givenToZeros

removeVarInit :: Ord v => Given v -> Maybe v -> ([Constraint v], [Constraint v])
removeVarInit (Given zs) v = partition (`hasVar` v) zs

removeVar1 :: Ord v => Constraint v -> Maybe v -> Constraint v -> Either (Constraint v) (Constraint v)
removeVar1 z0 v z = case Z.removeVar z0 z v of
	Just z' -> Left z'; Nothing -> Right z

removeVarStep :: Ord v => Maybe v -> [Constraint v] -> ([Constraint v], [Constraint v])
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
