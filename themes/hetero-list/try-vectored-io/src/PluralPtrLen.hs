{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeFamilies, TypeFamilyDependencies, GADTs #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module PluralPtrLen (
	PluralPtrLen(..), PtrLenList(..), PtrLenTuple(..), ListTuple(..) ) where

import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable, sizeOf)
import Foreign.Marshal (allocaArray, peekArray, pokeArray)
import Foreign.C.Types (CChar)
import Control.Monad (zipWithM_)

infixr 5 :-, :--, :.

data PtrLenList a = PtrLenListNil | (Ptr a, Int) :- PtrLenList a deriving Show

mapPtrLenList :: (Ptr a -> Int -> b) -> PtrLenList a -> [b]
mapPtrLenList _ PtrLenListNil = []
mapPtrLenList f ((p, n) :- pns) = f p n : mapPtrLenList f pns

allocaPtrLenList :: Storable a => [Int] -> (PtrLenList a -> IO b) -> IO b
allocaPtrLenList [] act = act PtrLenListNil
allocaPtrLenList (n : ns) act =
	allocaArray n $ \p -> allocaPtrLenList ns $ act . ((p, n) :-)

data PtrLenTuple :: [*] -> * where
	PtrLenTupleNil :: PtrLenTuple '[]
	(:--) :: (Ptr a, Int) -> PtrLenTuple as -> PtrLenTuple (a : as)

data ListTuple :: [*] -> * where
	ListTupleNil :: ListTuple '[]
	(:.) :: [a] -> ListTuple as -> ListTuple (a : as)

class PluralPtrLen ppl where
	type ValueLists ppl = vls | vls -> ppl
	toCCharPtrLenList :: ppl -> [(Ptr CChar, Int)]
	allocaPluralPtrLen :: [Int] -> (ppl -> IO a) -> IO a
	peekPluralPtrLen :: ppl -> IO (ValueLists ppl)
	pokePluralPtrLen :: ppl -> ValueLists ppl -> IO ()
	valueListLengths :: ValueLists ppl -> [Int]

instance Storable a => PluralPtrLen (PtrLenList a) where
	type ValueLists (PtrLenList a) = [[a]]
	toCCharPtrLenList = mapPtrLenList $ \p n -> (castPtr p, n * sizeOfPtr p)
	allocaPluralPtrLen = allocaPtrLenList
	peekPluralPtrLen = sequence . mapPtrLenList (flip peekArray)
	pokePluralPtrLen = zipWithM_ ($) . mapPtrLenList (const . pokeArray)
	valueListLengths = (length <$>)

instance PluralPtrLen (PtrLenTuple '[]) where
	type ValueLists (PtrLenTuple '[]) = ListTuple '[]
	toCCharPtrLenList _ = []
	allocaPluralPtrLen _ = ($ PtrLenTupleNil)
	peekPluralPtrLen _ = return ListTupleNil
	pokePluralPtrLen _ _ = return ()
	valueListLengths _ = []

instance (Storable a, PluralPtrLen (PtrLenTuple as),
			ValueLists (PtrLenTuple as) ~ ListTuple as) =>
		PluralPtrLen (PtrLenTuple (a : as)) where
	type ValueLists (PtrLenTuple (a : as)) = ListTuple (a : as)
	toCCharPtrLenList ((p, n) :-- pns) =
		(castPtr p, n * sizeOfPtr p) : toCCharPtrLenList pns
	allocaPluralPtrLen [] _ = error errMsgNotEnoughLength
	allocaPluralPtrLen (n : ns) act =
		allocaArray n $ \p -> allocaPluralPtrLen ns $ act . ((p, n) :--)
	peekPluralPtrLen ((p, n) :-- pns) =
		(:.) <$> peekArray n p <*> peekPluralPtrLen pns
	pokePluralPtrLen ((p, _n) :-- pns) (v :. vs) =
		pokeArray p v >> pokePluralPtrLen pns vs
	valueListLengths (vl :. vls) = length vl : valueListLengths vls

sizeOfPtr :: forall a . Storable a => Ptr a -> Int
sizeOfPtr _ = sizeOf @a undefined

errMsgNotEnoughLength :: String
errMsgNotEnoughLength = "module PluralPtrLen: " ++
	"instance PluralPtrLen (PtrLenTuple (a : as)): " ++
	"PluralPtrLen.allocaPluralPtrLen: need more lengths"
