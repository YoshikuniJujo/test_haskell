{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module IterateeWithIdState where

import IdState

data Iteratee a b = Done b | Get Id (a -> Iteratee a b)

-- instance Functor (Iteratee a) where
	
