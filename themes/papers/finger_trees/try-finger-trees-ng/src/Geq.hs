{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Geq where

import Outputable

import Expression
import Equal

newtype Geq i v = Geq (Expression i v) deriving (Show, Eq, Outputable)

infix 4 .>=

(.>=) :: (Integral i, Ord v) => Expression i v -> Expression i v -> Geq i v
e1 .>= e2 = Geq . reduct $ e1 .- e2

includeVarGeq :: Ord v => Geq i v -> Maybe v -> Bool
includeVarGeq (Geq e) = includeVar e

variablesGeq :: Geq i v -> [Maybe v]
variablesGeq (Geq e) = variables e

nullGeq :: Geq i v -> Bool
nullGeq (Geq e) = nullExpression e

annihilationGeqEq :: (Integral i, Ord v) => Geq i v -> Equal i v -> Maybe v -> Maybe (Geq i v)
annihilationGeqEq (Geq e1) (Equal e2) nv = Geq <$> annihilationIeqEq e1 e2 nv

annihilationGeqGeq :: (Integral i, Ord v) => Geq i v -> Geq i v -> Maybe v -> Maybe (Geq i v)
annihilationGeqGeq (Geq e1) (Geq e2) nv = Geq <$> annihilationIeqIeq e1 e2 nv
