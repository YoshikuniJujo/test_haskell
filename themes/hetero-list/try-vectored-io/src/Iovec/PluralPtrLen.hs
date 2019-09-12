{-# LANGUAGE TypeFamilies, TypeFamilyDependencies, GADTs #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Iovec.PluralPtrLen (
	Iovec(..),
	PluralPtrLen(..), peekByteStringPluralPtrLen, pluralPtrLenByteLength,
	PtrLenList(..), PtrLenTuple(..), ListTuple(..) ) where

import GHC.Stack (HasCallStack)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable)
import Foreign.Marshal (allocaArray, peekArray, pokeArray)
import Foreign.C.String (CStringLen)
import Control.Monad (zipWithM_)

import qualified Data.ByteString as BS

import Iovec.Iovec (Iovec(..))
import Tools (sizeOfPtr, errorWithExpression)

infixr 5 :-, :--, :.

class PluralPtrLen ppl where
	type ValueLists ppl = vls | vls -> ppl
	toIovecList :: ppl -> [Iovec]
	allocaPluralPtrLen :: HasCallStack => [Int] -> (ppl -> IO a) -> IO a
	peekPluralPtrLen :: ppl -> IO (ValueLists ppl)
	pokePluralPtrLen :: ppl -> ValueLists ppl -> IO ()
	valueListLengthList :: ValueLists ppl -> [Int]

pluralPtrLenByteLength :: PluralPtrLen ppl => ppl -> Int
pluralPtrLenByteLength =
	sum . ((\(Iovec _ l) -> fromIntegral l) <$>) . toIovecList

peekByteStringPluralPtrLen ::
	(PluralPtrLen ppl, HasCallStack) => ppl -> Int -> IO [BS.ByteString]
peekByteStringPluralPtrLen ppl n =
	(BS.packCStringLen `mapM`) . takeCStringLen n $ toIovecList ppl

takeCStringLen :: HasCallStack => Int -> [Iovec] -> [CStringLen]
takeCStringLen 0 _ = []
takeCStringLen _ [] = errorWithExpression
	"peekByteStringPluralPtrLen ppl n"
	"n should be less than byte length of ppl"
takeCStringLen tn _ | tn < 0 = errorWithExpression
	"peekByteStringPluralPtrLen ppl n"
	("n should be positive integer: " ++ show tn)
takeCStringLen tn (Iovec pc n_ : iovs)
	| tn <= n = [(pc, tn)]
	| otherwise = (pc, n) : takeCStringLen (tn - n) iovs
	where n = fromIntegral n_

data PtrLenList a = PtrLenListNil | (Ptr a, Int) :- PtrLenList a deriving Show

mapPtrLenList :: (Ptr a -> Int -> b) -> PtrLenList a -> [b]
_ `mapPtrLenList` PtrLenListNil = []
f `mapPtrLenList` ((p, n) :- pns) = f p n : f `mapPtrLenList` pns

allocaPtrLenList :: Storable a => [Int] -> (PtrLenList a -> IO b) -> IO b
allocaPtrLenList [] act = act PtrLenListNil
allocaPtrLenList (n : ns) act =
	allocaArray n $ \p -> allocaPtrLenList ns $ act . ((p, n) :-)

instance Storable a => PluralPtrLen (PtrLenList a) where
	type ValueLists (PtrLenList a) = [[a]]
	toIovecList = mapPtrLenList $ \p n ->
		Iovec (castPtr p) (fromIntegral $ n * sizeOfPtr p)
	allocaPluralPtrLen = allocaPtrLenList
	peekPluralPtrLen = sequence . mapPtrLenList (flip peekArray)
	pokePluralPtrLen = zipWithM_ ($) . mapPtrLenList (const . pokeArray)
	valueListLengthList = (length <$>)

data PtrLenTuple :: [*] -> * where
	PtrLenTupleNil :: PtrLenTuple '[]
	(:--) :: (Ptr a, Int) -> PtrLenTuple as -> PtrLenTuple (a : as)

instance Show (PtrLenTuple '[]) where show PtrLenTupleNil = "PtrLenTupleNil"
deriving instance (Show a, Show (PtrLenTuple as)) => Show (PtrLenTuple (a : as))

instance PluralPtrLen (PtrLenTuple '[]) where
	type ValueLists (PtrLenTuple '[]) = ListTuple '[]
	toIovecList _ = []
	allocaPluralPtrLen _ = ($ PtrLenTupleNil)
	peekPluralPtrLen _ = return ListTupleNil
	pokePluralPtrLen _ _ = return ()
	valueListLengthList _ = []

instance (Storable a, PluralPtrLen (PtrLenTuple as),
			ValueLists (PtrLenTuple as) ~ ListTuple as) =>
		PluralPtrLen (PtrLenTuple (a : as)) where
	type ValueLists (PtrLenTuple (a : as)) = ListTuple (a : as)
	toIovecList ((p, n) :-- pns) =
		Iovec (castPtr p) (fromIntegral $ n * sizeOfPtr p) :
		toIovecList pns
	allocaPluralPtrLen [] _ = errorWithExpression
		("instance PluralPtrLen (PtrLenTuple (a : as)): " ++ 
			"allocaPluralPtrLen ls act")
		"insufficient list `ls' element: need more lengths"
	allocaPluralPtrLen (n : ns) act =
		allocaArray n $ \p -> allocaPluralPtrLen ns $ act . ((p, n) :--)
	peekPluralPtrLen ((p, n) :-- pns) =
		(:.) <$> peekArray n p <*> peekPluralPtrLen pns
	pokePluralPtrLen ((p, _n) :-- pns) (v :. vs) =
		pokeArray p v >> pokePluralPtrLen pns vs
	valueListLengthList (vl :. vls) = length vl : valueListLengthList vls

data ListTuple :: [*] -> * where
	ListTupleNil :: ListTuple '[]
	(:.) :: [a] -> ListTuple as -> ListTuple (a : as)

instance Show (ListTuple '[]) where show ListTupleNil = "ListTupleNil"
deriving instance (Show a, Show (ListTuple as)) => Show (ListTuple (a : as))
