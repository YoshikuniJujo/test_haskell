{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Sorted (
	-- * Types
	Sorted(Nil), Numbered, numbered,
	-- * Type Level Operators
	Singleton, Insert, Merge, Map, (:-), (:+:), (:$:) ) where

import Data.Sorted.Internal
