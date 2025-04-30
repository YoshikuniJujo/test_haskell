{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DefaultSignatures #-}

module Class.Default where

class Foo a where
	f :: (a -> a) -> a -> a

--	default f :: Bar a => (a -> a) -> a -> a
--	f = g

class Bar a where
	g :: (forall b . b -> b) -> a -> a

	default g :: Foo a => (forall b . b -> b) -> a -> a
	g = f
