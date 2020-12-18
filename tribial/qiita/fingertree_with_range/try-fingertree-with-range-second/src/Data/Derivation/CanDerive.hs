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

newtype Wanted v = Wanted { unWanted :: [Wanted1 v] } deriving Show

type Wanted1 v = Constraint v

mkWanted :: Ord v => Exp v Bool -> Maybe (Wanted v)
mkWanted = uncurry wanted . mkConstraint empty

wanted :: Maybe (Wanted1 v) -> [Wanted1 v] -> Maybe (Wanted v)
wanted mw ws = Wanted . (: ws) <$> mw

rmVar :: Ord v => Given v -> Maybe v -> Given v
rmVar (Given g) v =
	Given . sort $ r ++ concat (fst $ unfoldUntil null (`rvStep` v) g')
	where (g', r) = partition (`hasVar` v) g

rvStep :: Ord v => [Constraint v] -> Maybe v -> ([Constraint v], [Constraint v])
rvStep [] _ = ([], [])
rvStep (c : cs) v = partitionEithers $ flip (rmVar1 c) v <$> cs

rmVar1 :: Ord v => Constraint v ->
	Constraint v -> Maybe v -> Either (Constraint v) (Constraint v)
rmVar1 c0 c v = maybe (Right c) Left $ C.rmVar c0 c v

unfoldUntil :: (s -> Bool) -> (s -> (r, s)) -> s -> ([r], s)
unfoldUntil p f s0
	| p s0 = ([], s0)
	| otherwise = let (r, s') = f s0 in (r :) `first` unfoldUntil p f s'

canDerive :: Ord v => Given v -> Wanted v -> Bool
canDerive g = all (canDerive1 g) . unWanted

canDerive1 :: Ord v => Given v -> Wanted1 v -> Bool
canDerive1 g w = selfContained w ||
	any (isDerivFrom w) (unGiven . foldl rmVar g $ givenVars g \\ getVars w)
