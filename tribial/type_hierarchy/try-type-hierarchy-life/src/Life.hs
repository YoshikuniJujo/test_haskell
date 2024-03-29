{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Life where

import Data.Typeable
import Data.Maybe

class (Typeable l, Show l) => Life l where
	toLife :: l -> SomeLife
	fromLife :: SomeLife -> Maybe l

	toLife = SomeLife
	fromLife (SomeLife l) = cast l

data SomeLife = forall l . Life l => SomeLife l deriving Typeable

instance Show SomeLife where
	showsPrec d (SomeLife l) =
		showParen (d > 10) (showString "SomeLife " . showsPrec 11 l)

instance Life SomeLife where
	toLife = id
	fromLife = Just

data Bacteria = Bacteria deriving (Typeable, Show)

instance Life Bacteria

data Animal = forall a . Life a => Animal a deriving Typeable

instance Life Animal

instance Show Animal where
	showsPrec d (Animal a) =
		showParen (d > 10) $ showString "Animal" . showsPrec 11 a

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

newtype Cat = Cat String deriving (Typeable, Show)

instance Life Cat where
	toLife = animalToLife
	fromLife = animalFromLife

data Human = forall h . Life h => Human h deriving Typeable

instance Life Human where
	toLife = animalToLife
	fromLife = animalFromLife

instance Show Human where
	showsPrec d (Human h) =
		showParen (d > 10) $ showString "Human " . showsPrec 11 h

humanToLife :: Life h => h -> SomeLife
humanToLife = toLife . Human

humanFromLife :: Life h => SomeLife -> Maybe h
humanFromLife l = do
	Human h <- fromLife l
	cast h

newtype Programmer = Programmer String deriving (Typeable, Show)

instance Life Programmer where
	toLife = humanToLife
	fromLife = humanFromLife

castLife :: (Life l1, Life l2) => l1 -> Maybe l2
castLife = fromLife . toLife

filterLife :: (Life l1, Life l2) => [l1] -> [l2]
filterLife = catMaybes . map castLife

withLifeIO :: (Life l1, Life l2) => l1 -> (l2 -> IO ()) -> IO ()
withLifeIO l f = maybe (pure ()) f $ castLife l
