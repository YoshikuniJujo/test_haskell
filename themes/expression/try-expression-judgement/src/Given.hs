{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Given where

import Prelude hiding ((<>))

import Data.Maybe
import Data.List

import Outputable

import Expression

newtype Given i v = Given [Expression i v] deriving (Show, Outputable)

allVariables :: Given i v -> [Maybe v]
allVariables (Given es) = variables =<< es

removeVar :: (Integral i, Ord v) => Given i v -> Maybe v -> Given i v
removeVar (Given es) = Given . remVar es

remVar :: (Integral i, Ord v) => [Expression i v] -> Maybe v -> [Expression i v]
remVar [] _ = []
remVar (e : es) v 
	| includeVar e v = remVar (remVarOf e v es) v
	| otherwise = e : remVar es v

remVarOf :: (Integral i, Ord v) => Expression i v -> Maybe v -> [Expression i v] -> [Expression i v] 
remVarOf e0 v es = replace (\e -> annihilation e e0 v) es

replace :: (a -> Maybe a) -> [a] -> [a]
replace _ [] = []
replace f (x : xs) = fromMaybe x (f x) : replace f xs

example :: Given Integer Char
example = Given [
	var 'm' .- num 1 .- var 'x',
	var 'm' .- num 3 .- var 'y',
	var 'x' .- num 1 .- var 'z',
	var 'z' .- num 1 .- var 'w',
	var 'w' .- num 1 .- var 'v' ]

newtype Wanted i v = Wanted (Expression i v) deriving (Show, Outputable)

wantedVariables :: Wanted i v -> [Maybe v]
wantedVariables (Wanted e) = variables e

exampleWanted :: Wanted Integer Char
exampleWanted = Wanted . reduct $ var 'y' .- num 1 .- var 'v'

canDerive :: (Show v, Show i, Integral i, Ord v) => Given i v -> Wanted i v -> Either String Bool
canDerive g (Wanted e0) | nullExpression e0 = Right True
canDerive g w@(Wanted e0) = case g' of
	Given [e] -> Right $ e == e0
	_ -> Left $ "Cannot derive " ++ show g ++ " " ++ show g' ++ " " ++ show w ++ " " ++ show (nub (allVariables g) \\ wantedVariables w)
	where
	g' = foldl removeVar g $ nub (allVariables g) \\ wantedVariables w
