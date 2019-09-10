{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeFamilies, GADTs, DataKinds, TypeOperators, KindSignatures #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module PluralPtrLen (PluralPtrLen(..), PtrLenList(..), PtrLenTuple(..), ListTuple(..)) where

import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable, sizeOf)
import Foreign.Marshal
import Foreign.C.Types (CChar)
import Control.Monad

infixr 5 :-, :--, :.

data PtrLenList a = PtrLenListNil | (Ptr a, Int) :- PtrLenList a deriving Show

mapPtrLenList :: (Ptr a -> Int -> b) -> PtrLenList a -> [b]
mapPtrLenList _ PtrLenListNil = []
mapPtrLenList f ((p, n) :- pns) = f p n : mapPtrLenList f pns

allocaPtrLenList :: Storable a => [Int] -> (PtrLenList a -> IO b) -> IO b
allocaPtrLenList [] act = act PtrLenListNil
allocaPtrLenList (n : ns) act =
	allocaArray n $ \p -> allocaPtrLenList ns $ \pll -> act ((p, n) :- pll)

data PtrLenTuple :: [*] -> * where
	PtrLenTupleNil :: PtrLenTuple '[]
	(:--) :: (Ptr a, Int) -> PtrLenTuple as -> PtrLenTuple (a : as)

data ListTuple :: [*] -> * where
	ListTupleNil :: ListTuple '[]
	(:.) :: [a] -> ListTuple as -> ListTuple (a : as)

class PluralPtrLen pl where
	type Elems pl = r | r -> pl
	toCCharPtrLenList :: pl -> [(Ptr CChar, Int)]
	elemsLength :: Elems pl -> [Int]
	allocaPluralPtrLen :: [Int] -> (pl -> IO a) -> IO a
	peekPluralPtrLen :: pl -> IO (Elems pl)
	pokePluralPtrLen :: pl -> Elems pl -> IO ()

instance Storable a => PluralPtrLen (PtrLenList a) where
	type Elems (PtrLenList a) = [[a]]
	toCCharPtrLenList = mapPtrLenList $ \p n -> (castPtr p, n * sizeOfPtr p)
	elemsLength = (length <$>)
	allocaPluralPtrLen ns act = allocaPtrLenList ns act
	peekPluralPtrLen = sequence . mapPtrLenList (flip peekArray)
	pokePluralPtrLen pl es = zipWithM_ ($) (mapPtrLenList (const . pokeArray) pl) es

instance PluralPtrLen (PtrLenTuple '[]) where
	type Elems (PtrLenTuple '[]) = ListTuple '[]
	toCCharPtrLenList _ = []
	elemsLength _ = []
	allocaPluralPtrLen _ act = act PtrLenTupleNil
	peekPluralPtrLen _ = return ListTupleNil
	pokePluralPtrLen _ _ = return ()

instance (Storable a, PluralPtrLen (PtrLenTuple as),
			Elems (PtrLenTuple as) ~ ListTuple as) =>
		PluralPtrLen (PtrLenTuple (a : as)) where
	type Elems (PtrLenTuple (a : as)) = ListTuple (a : as)
	toCCharPtrLenList ((p, n) :-- pns) =
		(castPtr p, n * sizeOfPtr p) : toCCharPtrLenList pns
	elemsLength (xs :. xss) = length xs : elemsLength xss
	allocaPluralPtrLen [] _ = error "not enough length"
	allocaPluralPtrLen (n : ns) act =
		allocaArray n $ \p -> allocaPluralPtrLen ns $ \plt -> act ((p, n) :-- plt)
	peekPluralPtrLen ((p, n) :-- pns) =
		(:.) <$> peekArray n p <*> peekPluralPtrLen pns
	pokePluralPtrLen ((p, _n) :-- pns) (v :. vs) = do
		pokeArray p v
		pokePluralPtrLen pns vs

sizeOfPtr :: forall a . Storable a => Ptr a -> Int
sizeOfPtr _ = sizeOf @a undefined
