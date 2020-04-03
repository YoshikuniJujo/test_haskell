{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Type.Set (
	-- * Types
	Set(Nil), Numbered, numbered,
	-- * Type Level Operators
	Singleton, Insert, Merge, Map, (:-), (:+:), (:$:) ) where

import Data.Type.Set.Internal
