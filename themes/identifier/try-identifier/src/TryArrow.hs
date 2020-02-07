{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryArrow where

import Control.Category
import Control.Arrow

data Iteratee a b = Done b | Get (a -> Iteratee a b)

{-
instance Category Iteratee where
	id = Get Done
	Done x . Done y = 
	-}
