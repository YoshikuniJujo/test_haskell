{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable #-}

import Data.Typeable
import Data.Maybe

data SomeLife = forall l . Life l => SomeLife l
	deriving Typeable

instance Show SomeLife where
	show (SomeLife l) = "SomeLife " ++ show l

class (Typeable l, Show l) => Life l where
	toLife :: l -> SomeLife
	fromLife :: SomeLife -> Maybe l

	toLife = SomeLife
	fromLife (SomeLife l) = cast l

instance Life SomeLife where
	toLife = id
	fromLife = Just

data Bacteria = Bacteria deriving (Typeable, Show)

instance Life Bacteria

data Fungus = Fungus deriving (Typeable, Show)

instance Life Fungus

castLife :: (Life l1, Life l2) => l1 -> Maybe l2
castLife = fromLife . toLife

filterLife :: (Life l1, Life l2) => [l1] -> [l2]
filterLife = catMaybes . map castLife

withLifeIO :: (Life l1, Life l2) => l1 -> (l2 -> IO ()) -> IO ()
withLifeIO l f = maybe (return ()) f $ castLife l

withYourFavoriteLife :: Life l => (l -> IO ()) -> IO ()
withYourFavoriteLife act = do
	withLifeIO Bacteria act
	withLifeIO Fungus act

main :: IO ()
main = do
	let	sls :: Life l => [l]
		sls = filterLife [
			SomeLife Bacteria,
			SomeLife Fungus,
			SomeLife Fungus ]
	print (sls :: [SomeLife])
	print (sls :: [Bacteria])
	print (sls :: [Fungus])
	withYourFavoriteLife (print :: SomeLife -> IO ())
	withYourFavoriteLife (print :: Bacteria -> IO ())
	withYourFavoriteLife (print :: Fungus -> IO ())
