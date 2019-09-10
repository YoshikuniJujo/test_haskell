{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs, DataKinds, TypeOperators, KindSignatures #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module PtrLenList (AsCCharPtrLenList(..), PtrLenList(..), PtrLenTuple(..)) where

import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable, sizeOf)
import Foreign.C.Types (CChar)

infixr 5 :-, :--

data PtrLenList a = PtrLenListNil | (Ptr a, Int) :- PtrLenList a deriving Show

mapPtrLenList :: (Ptr a -> Int -> b) -> PtrLenList a -> [b]
mapPtrLenList _ PtrLenListNil = []
mapPtrLenList f ((p, n) :- pns) = f p n : mapPtrLenList f pns

data PtrLenTuple :: [*] -> * where
	PtrLenTupleNil :: PtrLenTuple '[]
	(:--) :: (Ptr a, Int) -> PtrLenTuple as -> PtrLenTuple (a : as)

class AsCCharPtrLenList pl where
	toCCharPtrLenList :: pl -> [(Ptr CChar, Int)]

instance Storable a => AsCCharPtrLenList (PtrLenList a) where
	toCCharPtrLenList = mapPtrLenList $ \p n -> (castPtr p, n * sizeOfPtr p)

instance AsCCharPtrLenList (PtrLenTuple '[]) where toCCharPtrLenList _ = []
instance (Storable a, AsCCharPtrLenList (PtrLenTuple as)) =>
		AsCCharPtrLenList (PtrLenTuple (a : as)) where
	toCCharPtrLenList ((p, n) :-- pns) =
		(castPtr p, n * sizeOfPtr p) : toCCharPtrLenList pns

sizeOfPtr :: forall a . Storable a => Ptr a -> Int
sizeOfPtr _ = sizeOf @a undefined
