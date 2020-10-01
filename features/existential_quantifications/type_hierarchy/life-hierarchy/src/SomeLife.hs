{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module SomeLife where

import Data.Typeable

data SomeLife = forall l . Life l => SomeLife l deriving Typeable

instance Show SomeLife where
	showsPrec d (SomeLife l) = showsPrec d l
--	showsPrec d (SomeLife l) = showParen (d > 10) $
--		showString "SomeLife " . showsPrec 11 l
--		showsPrec 11 l

class (Typeable l, Show l) => Life l where
	toLife :: l -> SomeLife
	fromLife :: SomeLife -> Maybe l

	toLife = SomeLife
	fromLife (SomeLife l) = cast l

instance Life SomeLife where
	toLife = id
	fromLife = Just

{-
data Animal = forall a . Life a => Animal a deriving Typeable

instance Life Animal

instance Show Animal where
	showsPrec d (Animal a) = showParen (d > 10) $
		showString "Animal " . showsPrec 11 a

animalToLife :: Life a => a -> SomeLife
animalToLife = toLife . Animal

animalFromLife :: Life a => SomeLife -> Maybe a
animalFromLife l = do
	Animal a <- fromLife l
	cast a

newtype Dog = Dog String deriving (Typeable, Show)

instance Life Dog where
	toLife = animalToLife
	fromLife = animalFromLife
	-}

castLife :: (Life l1, Life l2) => l1 -> Maybe l2
castLife = fromLife . toLife
