{-# LANGUAGE ExistentialQuantification, GADTs, RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Fun (
	Fun(..), Tag(..), Tg(..), MaybeTg(..), Boolean(..)
	) where

class Fun f where
	fun :: (a -> m b) -> f m a b
	($$) :: Applicative m => f m a b -> a -> m b

data MaybeTg a b c where
	N :: MaybeTg a b c
	J :: Tg -> MaybeTg a a a

data Boolean a b where
	F :: Boolean a b
	T :: Boolean a a

data Tg = Tg Integer Integer deriving Show

class Tag (t :: (* -> *) -> * -> * -> *) where
	open :: Integer -> t m a a
	next :: Tg -> t m a a
	close :: Integer -> t m a a
	checkOpen :: t m a b -> t m a c -> MaybeTg a b c
	checkClose :: Tg -> t m a b -> Boolean a b
