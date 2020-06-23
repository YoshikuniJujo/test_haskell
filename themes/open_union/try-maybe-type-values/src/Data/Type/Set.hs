{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Type.Set (
	-- * Type
	Set(Nil), Numbered, numbered,
	-- * Operator
	Singleton, Insert, Merge, Map, (:-), (:+:), (:$:) ) where

import Data.Type.Set.Internal (
	Set(Nil), Numbered, numbered,
	Singleton, Insert, Merge, Map, (:-), (:+:), (:$:) )
