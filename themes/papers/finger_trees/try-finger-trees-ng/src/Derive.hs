{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Derive where

import Prelude hiding ((<>))

import Control.Arrow (first)
import Data.Maybe
import Data.List

import Outputable

import Expression
import Equal
import Geq

data Given i v = Given [Equal i v] [Geq i v] deriving Show

instance (Outputable i, Outputable v) => Outputable (Given i v) where
	ppr (Given eqs geqs) = "(Given" <+> ppr eqs <+> ppr geqs <> ")"

allVariables :: Given i v -> [Maybe v]
allVariables (Given eqs geqs) = (variablesEq =<< eqs) ++ (variablesGeq =<< geqs)

removeVar :: (Integral i, Ord v) => Given i v -> Maybe v -> Given i v
removeVar (Given eqs geqs) v = Given eqs' geqs''
	where
	(eqs', geqs') = remVarEq v eqs geqs
	geqs'' = remVarGeq v geqs'

remVarEq :: (Integral i, Ord v) => Maybe v -> [Equal i v] -> [Geq i v] -> ([Equal i v], [Geq i v])
remVarEq _ [] geqs = ([], geqs)
remVarEq v (e : es) gs
	| includeVarEq e v = uncurry (remVarEq v) $ remVarOfEq e v es gs
	| otherwise = (e :) `first` remVarEq v es gs

remVarOfEq :: (Integral i, Ord v) => Equal i v -> Maybe v -> [Equal i v] -> [Geq i v] -> ([Equal i v], [Geq i v])
remVarOfEq e0 v eqs geqs = (eqs', geqs')
	where
	eqs' = replace (\e -> annihilationEq e e0 v) eqs
	geqs' = replace (\g -> annihilationGeqEq g e0 v) geqs

remVarGeq :: (Integral i, Ord v) => Maybe v -> [Geq i v] -> [Geq i v]
remVarGeq _ [] = []
remVarGeq v (g : gs)
	| includeVarGeq g v = remVarGeq v $ remVarOfGeq g v gs
	| otherwise = g : remVarGeq v gs

remVarOfGeq :: (Integral i, Ord v) => Geq i v -> Maybe v -> [Geq i v] -> [Geq i v]
remVarOfGeq g0 v = replace $ \g -> annihilationGeqGeq g g0 v

replace :: (a -> Maybe a) -> [a] -> [a]
replace _ [] = []
replace f (x : xs) = fromMaybe x (f x) : replace f xs

data Wanted i v = WantedEq (Equal i v) | WantedGeq (Geq i v) deriving Show

instance (Outputable i, Outputable v) => Outputable (Wanted i v) where
	ppr (WantedEq eq) = "(WantedEq" <+> ppr eq <> ")"
	ppr (WantedGeq geq) = "(WantedGeq" <+> ppr geq <> ")"

wantedVariables :: Wanted i v -> [Maybe v]
wantedVariables (WantedEq eq) = variablesEq eq
wantedVariables (WantedGeq geq) = variablesGeq geq

deriveFrom :: (Integral i, Ord v) => Given i v -> Wanted i v -> Given i v
deriveFrom g w = foldl removeVar g $ nub (allVariables g) \\ wantedVariables w

exampleGiven :: Given Integer String
exampleGiven = Given [
	var "monkey" .+ var "m'" .= var "d",
	var "m'" .- num 1 .= var "f'" ] [
	var "d" .>= num 1,
	var "monkey" .>= num 1 ]

exampleWanted :: Wanted Integer String
exampleWanted = WantedGeq $ ((var "monkey" .- num 1) .+ var "m'") .+ num 1 .>= num 1

canDerive :: (Integral i, Ord v) => Given i v -> Wanted i v -> Bool
canDerive _ (WantedEq eq) | nullEqual eq = True
canDerive _ (WantedGeq geq) | nullGeq geq = True
canDerive g w = case (g', w) of
	(Given eqs _, WantedEq eq) -> eq `elem` eqs
	(Given _ geqs, WantedGeq geq) -> geq `elem` geqs
	where g' = deriveFrom g w
