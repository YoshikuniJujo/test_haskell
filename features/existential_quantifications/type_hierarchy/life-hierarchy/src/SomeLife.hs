{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module SomeLife where

import Data.Typeable
import Language.Haskell.TH

data SomeLife = forall l . Life l => SomeLife l deriving Typeable

instance Show SomeLife where
	showsPrec d (SomeLife l) = showParen (d > 10) $
		showString "SomeLife " . showsPrec 11 l

class (Typeable l, Show l) => Life l where
	toLife :: l -> SomeLife
	fromLife :: SomeLife -> Maybe l

	toLife = SomeLife
	fromLife (SomeLife l) = cast l

instance Life SomeLife where
	toLife = id
	fromLife = Just

data LifeHierarchy
	= LifeNode String [LifeHierarchy]
	| LifeType Name
	deriving Show

lifeContainer :: Name -> DecsQ
lifeContainer lc = sequence [
	]
