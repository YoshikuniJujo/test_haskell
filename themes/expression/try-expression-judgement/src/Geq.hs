{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Geq where

import Outputable

import Expression

newtype Geq i v = Geq (Expression i v) deriving (Show, Eq, Outputable)

infix 4 .>=

(.>=) :: (Integral i, Ord v) => Expression i v -> Expression i v -> Geq i v
e1 .>= e2 = Geq . reduct $ e1 .- e2
