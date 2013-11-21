{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable #-}

import Data.Typeable
import Data.Maybe

data SomeLife = forall l . Life l => SomeLife l deriving Typeable

class (Typeable l, Show l) => Life l where
	toLife :: l -> SomeLife
	fromLife :: SomeLife -> Maybe l

	toLife = SomeLife
	fromLife (SomeLife l) = cast l

instance Show SomeLife where
	showsPrec d (SomeLife l) = showParen (d > app_prec) $
		showString "SomeLife " . showsPrec (app_prec + 1) l
		where
		app_prec = 10

instance Life SomeLife where
	toLife = id
	fromLife = Just

data Animal = forall a . Life a => Animal a deriving Typeable

instance Show Animal where
	showsPrec d (Animal a) = showParen (d > app_prec) $
		showString "Animal " . showsPrec (app_prec + 1) a
		where
		app_prec = 10

instance Life Animal

animalToLife :: Life a => a -> SomeLife
animalToLife = toLife . Animal

animalFromLife :: Life a => SomeLife -> Maybe a
animalFromLife l = do
	Animal a <- fromLife l
	cast a

data Dog = Dog String deriving (Typeable, Show)

instance Life Dog where
	toLife = animalToLife
	fromLife = animalFromLife

data Cat = Cat String deriving (Typeable, Show)

instance Life Cat where
	toLife = animalToLife
	fromLife = animalFromLife

castLife :: (Life l, Life m) => l -> Maybe m
castLife = fromLife . toLife

test :: [SomeLife]
test = [toLife $ Dog "pochi", toLife $ Cat "mike"]

filterLife :: Life l => [SomeLife] -> [l]
filterLife = catMaybes . map castLife
