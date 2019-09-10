{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs, KindSignatures, DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module PtrLenList where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types

data PtrLenList a = PtrLenNil | (Ptr a, Int) :- PtrLenList a deriving Show

lengthPtrLenList :: Num n => PtrLenList a -> n
lengthPtrLenList PtrLenNil = 0
lengthPtrLenList (_ :- pns) = 1 + lengthPtrLenList pns

mapPtrLenList :: (Ptr a -> Int -> b) -> PtrLenList a -> [b]
mapPtrLenList _ PtrLenNil = []
mapPtrLenList f ((p, n) :- pns) = f p n : mapPtrLenList f pns

infixr 5 :-

data HeteroPtrLenList :: [*] -> * where
	HtrPtrLenNil :: HeteroPtrLenList '[]
	(:--) :: (Ptr a, Int) ->
		HeteroPtrLenList as -> HeteroPtrLenList (a : as)

infixr 5 :--

lengthHeteroPtrLenList :: Num n => HeteroPtrLenList a -> n
lengthHeteroPtrLenList HtrPtrLenNil = 0
lengthHeteroPtrLenList (_ :-- pns) = 1 + lengthHeteroPtrLenList pns

class AsCCharPtrLenList pl where
	toCCharPtrLenList :: pl -> [(Ptr CChar, Int)]

instance Storable a => AsCCharPtrLenList (PtrLenList a) where
	toCCharPtrLenList = mapPtrLenList $ \p n -> (castPtr p, n * sizeOfPtr p)

instance AsCCharPtrLenList (HeteroPtrLenList '[]) where
	toCCharPtrLenList _ = []

instance (Storable a, AsCCharPtrLenList (HeteroPtrLenList as)) =>
		AsCCharPtrLenList (HeteroPtrLenList (a : as)) where
	toCCharPtrLenList ((p, n) :-- pns) =
		(castPtr p, n * sizeOfPtr p) : toCCharPtrLenList pns

sizeOfPtr :: forall a . Storable a => Ptr a -> Int
sizeOfPtr _ = sizeOf @a undefined
