{-# LANGUAGE TypeFamilies, TypeFamilyDependencies, GADTs #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Iovec.PluralArray (
	Iovec(..),
	PluralArray(..), peekByteStringPluralArray, pluralArrayByteLength,
	ArrayList(..), ArrayTuple(..), ListTuple(..) ) where

import GHC.Stack (HasCallStack)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable)
import Foreign.Marshal (allocaArray, peekArray, pokeArray, copyBytes, fillBytes)
import Foreign.C.Types (CChar)
import Foreign.C.String (CStringLen)
import Control.Arrow (second)
import Control.Monad (when, zipWithM_)

import qualified Data.ByteString as BS

import Iovec.Iovec (Iovec(..))
import Tools (for2M_, sizeOfPtr, errorWithExpression)

infixr 5 :-, :--, :.

type Array a = (Ptr a, Int)

class PluralArray ppl where
	type ValueLists ppl = vls | vls -> ppl
	toIovecList :: ppl -> [Iovec]
	allocaPluralArray :: HasCallStack => [Int] -> (ppl -> IO a) -> IO a
	peekPluralArray :: ppl -> IO (ValueLists ppl)
	pokePluralArray :: ppl -> ValueLists ppl -> IO ()
	valueListLengthList :: ValueLists ppl -> [Int]

pluralArrayByteLength :: PluralArray ppl => ppl -> Int
pluralArrayByteLength =
	sum . ((\(Iovec _ l) -> fromIntegral l) <$>) . toIovecList

peekByteStringPluralArray ::
	(PluralArray ppl, HasCallStack) => ppl -> Int -> IO [BS.ByteString]
peekByteStringPluralArray ppl n =
	(BS.packCStringLen `mapM`) . takeCStringLen n $ toIovecList ppl

takeCStringLen :: HasCallStack => Int -> [Iovec] -> [CStringLen]
takeCStringLen 0 _ = []
takeCStringLen _ [] = errorWithExpression
	"peekByteStringPluralArray ppl n"
	"n should be less than byte length of ppl"
takeCStringLen tn _ | tn < 0 = errorWithExpression
	"peekByteStringPluralArray ppl n"
	("n should be positive integer: " ++ show tn)
takeCStringLen tn (Iovec pc n_ : iovs)
	| tn <= n = [(pc, tn)]
	| otherwise = (pc, n) : takeCStringLen (tn - n) iovs
	where n = fromIntegral n_

data ArrayList a = ArrayListNil | Array a :- ArrayList a deriving Show

mapArrayList :: (Ptr a -> Int -> b) -> ArrayList a -> [b]
_ `mapArrayList` ArrayListNil = []
f `mapArrayList` ((p, n) :- pns) = f p n : f `mapArrayList` pns

{-
arrayListToList :: ArrayList a -> [(Ptr a, Int)]
arrayListToList ArrayListNil = []
arrayListToList (pn :- pns) = pn : arrayListToList pns
-}

listToArrayList :: [Array a] -> ArrayList a
listToArrayList [] = ArrayListNil
listToArrayList (pn : pns) = pn :- listToArrayList pns

instance Storable a => PluralArray (ArrayList a) where
	type ValueLists (ArrayList a) = [[a]]
	toIovecList = mapArrayList $ \p n ->
		Iovec (castPtr p) (fromIntegral $ n * sizeOfPtr p)
	allocaPluralArray foo act = allocaArrayList foo $ act . listToArrayList
	peekPluralArray = sequence . mapArrayList (flip peekArray)
	pokePluralArray = zipWithM_ ($) . mapArrayList (const . pokeArray)
	valueListLengthList = (length <$>)

data ArrayTuple :: [*] -> * where
	ArrayTupleNil :: ArrayTuple '[]
	(:--) :: Array a -> ArrayTuple as -> ArrayTuple (a : as)

instance Show (ArrayTuple '[]) where show ArrayTupleNil = "ArrayTupleNil"
deriving instance (Show a, Show (ArrayTuple as)) => Show (ArrayTuple (a : as))

instance PluralArray (ArrayTuple '[]) where
	type ValueLists (ArrayTuple '[]) = ListTuple '[]
	toIovecList _ = []
	allocaPluralArray _ = ($ ArrayTupleNil)
	peekPluralArray _ = return ListTupleNil
	pokePluralArray _ _ = return ()
	valueListLengthList _ = []

instance (Storable a, PluralArray (ArrayTuple as),
			ValueLists (ArrayTuple as) ~ ListTuple as) =>
		PluralArray (ArrayTuple (a : as)) where
	type ValueLists (ArrayTuple (a : as)) = ListTuple (a : as)
	toIovecList ((p, n) :-- pns) =
		Iovec (castPtr p) (fromIntegral $ n * sizeOfPtr p) :
		toIovecList pns
	allocaPluralArray [] _ = errorWithExpression
		("instance PluralArray (ArrayTuple (a : as)): " ++ 
			"allocaPluralArray ls act")
		"insufficient list `ls' element: need more lengths"
	allocaPluralArray (n : ns) act =
		allocaArray n $ \p -> allocaPluralArray ns $ act . ((p, n) :--)
	peekPluralArray ((p, n) :-- pns) =
		(:.) <$> peekArray n p <*> peekPluralArray pns
	pokePluralArray ((p, _n) :-- pns) (v :. vs) =
		pokeArray p v >> pokePluralArray pns vs
	valueListLengthList (vl :. vls) = length vl : valueListLengthList vls

data ListTuple :: [*] -> * where
	ListTupleNil :: ListTuple '[]
	(:.) :: [a] -> ListTuple as -> ListTuple (a : as)

instance Show (ListTuple '[]) where show ListTupleNil = "ListTupleNil"
deriving instance (Show a, Show (ListTuple as)) => Show (ListTuple (a : as))

newtype ByteArrayList =
	ByteArrayList { getByteArrayList :: [Array CChar] } deriving Show

allocaArrayList :: Storable a => [Int] -> ([Array a] -> IO b) -> IO b
allocaArrayList [] act = act []
allocaArrayList (n : ns) act =
	allocaArray n $ \p -> allocaArrayList ns $ act . ((p, n) :)

instance PluralArray ByteArrayList where
	type ValueLists ByteArrayList = [BS.ByteString]
	toIovecList =
		(uncurry Iovec . second fromIntegral <$>) . getByteArrayList
	allocaPluralArray ns act = allocaArrayList ns $ act . ByteArrayList
	peekPluralArray = (BS.packCStringLen `mapM`) . getByteArrayList
	pokePluralArray (ByteArrayList bll) bss = for2M_ bll bss $ \(dpc, dn) bs ->
		BS.useAsCStringLen bs $ \(spc, sn) -> do
			copyBytes dpc spc $ min dn sn
			when (dn > sn) $ fillBytes (dpc `plusPtr` sn) 0 (dn - sn)
	valueListLengthList = (BS.length <$>)
