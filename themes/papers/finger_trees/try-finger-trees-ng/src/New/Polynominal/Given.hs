{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module New.Polynominal.Given where

import Data.List

import New.Polynominal.Zero

newtype Given v = Given [Zero v] deriving Show

given :: Ord v => [Zero v] -> Given v
given = Given . nub . sort
