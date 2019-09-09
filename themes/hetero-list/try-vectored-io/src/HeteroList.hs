{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module HeteroList (
	HeteroList(..), HeteroPtrList(..),
	lengthHeteroList, lengthHeteroPtrList, StorableHeteroList(..)) where

import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable, peek, poke)

data HeteroList :: [*] -> * where
	Nil :: HeteroList '[]
	(:-) :: a -> HeteroList as -> HeteroList (a : as)

infixr 5 :-

lengthHeteroList :: HeteroList a -> Int
lengthHeteroList Nil = 0
lengthHeteroList (_ :- xs) = 1 + lengthHeteroList xs

instance Show (HeteroList '[]) where
	show Nil = "Nil"

instance (Show a, Show (HeteroList as)) => Show (HeteroList (a : as)) where
	show (x :- xs) = show x ++ " :- " ++ show xs

data HeteroPtrList :: [*] -> * where
	PtrNil :: HeteroPtrList '[]
	(:--) :: Ptr a -> HeteroPtrList as -> HeteroPtrList (a : as)

infixr 5 :--

lengthHeteroPtrList :: HeteroPtrList a -> Int
lengthHeteroPtrList PtrNil = 0
lengthHeteroPtrList (_ :-- ps) = 1 + lengthHeteroPtrList ps

class StorableHeteroList a where
	peekHeteroList :: HeteroPtrList a -> IO (HeteroList a)
	pokeHeteroList :: HeteroPtrList a -> HeteroList a -> IO ()

instance StorableHeteroList '[] where
	peekHeteroList _ = return Nil
	pokeHeteroList _ _ = return ()

instance (Storable a, StorableHeteroList as) =>
		StorableHeteroList (a : as) where
	peekHeteroList (p :-- ps) = (:-) <$> peek p <*> peekHeteroList ps
	pokeHeteroList (p :-- ps) (x :- xs) = poke p x >> pokeHeteroList ps xs
