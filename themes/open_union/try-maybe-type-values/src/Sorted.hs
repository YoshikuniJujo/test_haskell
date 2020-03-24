{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Sorted (
	-- * Types
	Sorted(Nil), Numbered, numbered,
	-- * Type Level Operators
	Singleton, Insert, Merge, Map, (:-), (:+:), (:$:) ) where

import Sorted.Internal
