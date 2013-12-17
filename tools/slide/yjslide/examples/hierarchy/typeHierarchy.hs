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

data Bacteria = Bacteria deriving (Typeable, Show)

instance Life Bacteria

data Animal = forall a . Life a => Animal a deriving Typeable

instance Show Animal where
	showsPrec d (Animal a) = showParen (d > app_prec) $
		showString "Animal " . showsPrec (app_prec + 1) a
		where
		app_prec = 10

instance Life Animal

data Human = forall h . Life h => Human h deriving Typeable

instance Show Human where
	showsPrec d (Human h) = showParen (d > 10) $
		showString "Human " . showsPrec 11 h

instance Life Human where
	toLife = animalToLife
	fromLife = animalFromLife

data Plant = forall p . Life p => Plant p deriving Typeable

instance Show Plant where
	showsPrec d (Plant p) = showParen (d > 10) $
		showString "Plant " . showsPrec 11 p

instance Life Plant

animalToLife :: Life a => a -> SomeLife
animalToLife = toLife . Animal

animalFromLife :: Life a => SomeLife -> Maybe a
animalFromLife l = do
	Animal a <- fromLife l
	cast a

humanToLife :: Life h => h -> SomeLife
humanToLife = toLife . Human

humanFromLife :: Life h => SomeLife -> Maybe h
humanFromLife l = do
	Human h <- fromLife l
	cast h

plantToLife :: Life p => p -> SomeLife
plantToLife = toLife . Plant

plantFromLife :: Life p => SomeLife -> Maybe p
plantFromLife l = do
	Plant p <- fromLife l
	cast p

newtype Dog = Dog String deriving (Typeable, Show)

instance Life Dog where
	toLife = animalToLife
	fromLife = animalFromLife

data Cat = Cat String deriving (Typeable, Show)

instance Life Cat where
	toLife = animalToLife
	fromLife = animalFromLife

newtype Programmer = Programmer String deriving (Typeable, Show)

instance Life Programmer where
	toLife = humanToLife
	fromLife = humanFromLife

newtype Author = Author String deriving (Typeable, Show)

instance Life Author where
	toLife = humanToLife
	fromLife = humanFromLife

newtype Tree = Tree String deriving (Typeable, Show)

instance Life Tree where
	toLife = plantToLife
	fromLife = plantFromLife

data Grass = Grass deriving (Typeable, Show)

instance Life Grass where
	toLife = plantToLife
	fromLife = plantFromLife

castLife :: (Life l, Life m) => l -> Maybe m
castLife = fromLife . toLife

test :: [SomeLife]
test = [toLife $ Dog "pochi", toLife $ Cat "mike", toLife $ Tree "nire",
	toLife Grass, toLife Bacteria, toLife $ Programmer "Matz",
	toLife $ Author "Natsume Souseki"]

filterLife :: (Life l1, Life l2) => [l1] -> [l2]
filterLife = catMaybes . map castLife

withLifeIO :: (Life l1, Life l2) => l1 -> (l2 -> IO ()) -> IO ()
withLifeIO l f = maybe (return ()) f $ castLife l
