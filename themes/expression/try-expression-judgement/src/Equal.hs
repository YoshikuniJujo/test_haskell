{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Equal where

import Outputable

import Expression

newtype Equal i v = Equal (Expression i v) deriving (Show, Eq, Outputable)

infixl 6 .=

(.=) :: (Integral i, Ord v) => Expression i v -> Expression i v -> Equal i v
e1 .= e2 = Equal . reductAndNormalizeSign $ e1 .- e2

includeVarEq :: Ord v => Equal i v -> Maybe v -> Bool
includeVarEq (Equal e) = includeVar e

variablesEq :: Equal i v -> [Maybe v]
variablesEq (Equal e) = variables e

nullEqual :: Equal i v -> Bool
nullEqual (Equal e) = nullExpression e

annihilationEq :: (Integral i, Ord v) =>
	Equal i v -> Equal i v -> Maybe v -> Maybe (Equal i v)
annihilationEq (Equal e1) (Equal e2) nv = Equal <$> annihilation e1 e2 nv
