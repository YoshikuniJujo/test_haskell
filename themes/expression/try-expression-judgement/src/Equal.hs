{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Equal where

import Outputable

import Expression

newtype Equal i v = Equal (Expression i v) deriving (Show, Eq, Outputable)

infixl 6 .=

(.=) :: (Integral i, Ord v) => Expression i v -> Expression i v -> Equal i v
e1 .= e2 = Equal . reductAndNormalizeSign $ e1 .- e2
