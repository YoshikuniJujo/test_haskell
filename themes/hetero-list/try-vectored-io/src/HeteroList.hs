{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeOperators, TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module HeteroList (
	HeteroList(..), lengthHeteroList, StorableHeteroList(..), TypeMap) where

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

class StorableHeteroList a where
	peekHeteroList :: HeteroList (TypeMap Ptr a) -> IO (HeteroList a)
	pokeHeteroList :: HeteroList (TypeMap Ptr a) -> HeteroList a -> IO ()

-- type family TypeMap (a :: * -> *) (xs :: [*]) :: [*] where
type family TypeMap (a :: * -> *) (xs :: [*]) = r | r -> xs where
	TypeMap t '[] = '[]
	TypeMap t (x ': xs) = t x ': TypeMap t xs

instance StorableHeteroList '[] where
	peekHeteroList _ = return Nil
	pokeHeteroList _ _ = return ()

instance (Storable a, StorableHeteroList as) =>
		StorableHeteroList (a : as) where
	peekHeteroList (p :- ps) = (:-) <$> peek p <*> peekHeteroList ps
	pokeHeteroList (p :- ps) (x :- xs) = poke p x >> pokeHeteroList ps xs
