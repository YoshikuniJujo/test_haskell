{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Type.Set (
	-- * Set
	Set(Nil),
	-- * Numbered
	Numbered, numbered,
	-- * Function
	Singleton, Insert, Merge,
	-- * Operator
	(:-), (:+:) ) where

import Data.Type.Set.Internal (
	Set(Nil), Numbered, numbered, Singleton, Insert, Merge, (:-), (:+:) )
