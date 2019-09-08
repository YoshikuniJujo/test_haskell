{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeOperators, StandaloneDeriving, FlexibleInstances, FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal

data HeteroList :: [*] -> * where
	Nil :: HeteroList '[]
	(:-) :: a -> HeteroList as -> HeteroList (a : as)

instance Show (HeteroList '[]) where
	show Nil = "Nil"

instance (Show a, Show (HeteroList as)) => Show (HeteroList (a : as)) where
	show (x :- xs) = show x ++ " :- " ++ show xs

infixr 5 :-

data HeteroPtrList :: [*] -> * where
	PtrNil :: HeteroPtrList '[]
	(:--) :: Ptr a -> HeteroPtrList as -> HeteroPtrList (a : as)

infixr 5 :--

class StorableHeteroList a where
	peekHeteroList :: HeteroPtrList a -> IO (HeteroList a)
	pokeHeteroList :: HeteroPtrList a -> HeteroList a -> IO ()

instance StorableHeteroList '[] where
	peekHeteroList _ = return Nil
	pokeHeteroList _ _ = return ()

instance (Storable a, StorableHeteroList as) => StorableHeteroList (a : as) where
	peekHeteroList (p :-- ps) =
		(:-) <$> peek p <*> peekHeteroList ps
	pokeHeteroList (p :-- ps) (x :- xs) = do
		poke p x
		pokeHeteroList ps xs

allocaIntBool :: (Ptr Int -> Ptr Bool -> IO a) -> IO a
allocaIntBool act = alloca $ \pi -> alloca $ \pb -> act pi pb

sample :: Int -> Bool -> IO (Int, Bool)
sample i b = allocaIntBool $ \pi pb -> do
	pokeHeteroList (pi :-- pb :-- PtrNil) (i :- b :- Nil)
	i' :- b' :- Nil <- peekHeteroList (pi :-- pb :-- PtrNil)
	return (i', b')
