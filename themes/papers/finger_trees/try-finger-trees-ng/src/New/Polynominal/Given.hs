{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module New.Polynominal.Given where

import New.Polynominal.Zero

newtype Given v = Given [Zero v] deriving Show
