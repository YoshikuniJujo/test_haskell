{-# LANGUAGE TypeFamilies, TypeFamilyDependencies, GADTs #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Iovec.PluralPtrLen (
	Iovec(..),
	PluralPtrLen(..), byteLengthPluralPtrLen, peekByteStringPluralPtrLen,
	PtrLenList(..), PtrLenTuple(..), ListTuple(..) ) where

import GHC.Stack
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable)
import Foreign.Marshal (allocaArray, peekArray, pokeArray)
import Foreign.C.Types (CChar)
import Control.Monad (zipWithM_)

import qualified Data.ByteString as BS

import Iovec.Types (Iovec(..))
import Tools (sizeOfPtr, errorWithFunName)

infixr 5 :-, :--, :.

class PluralPtrLen ppl where
	type ValueLists ppl = vls | vls -> ppl
	toIovecList :: ppl -> [Iovec]
	allocaPluralPtrLen :: HasCallStack => [Int] -> (ppl -> IO a) -> IO a
	peekPluralPtrLen :: ppl -> IO (ValueLists ppl)
	pokePluralPtrLen :: ppl -> ValueLists ppl -> IO ()
	valueListLengths :: ValueLists ppl -> [Int]

data PtrLenList a = PtrLenListNil | (Ptr a, Int) :- PtrLenList a deriving Show

mapPtrLenList :: (Ptr a -> Int -> b) -> PtrLenList a -> [b]
mapPtrLenList _ PtrLenListNil = []
mapPtrLenList f ((p, n) :- pns) = f p n : mapPtrLenList f pns

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
	valueListLengths = (length <$>)

data PtrLenTuple :: [*] -> * where
	PtrLenTupleNil :: PtrLenTuple '[]
	(:--) :: (Ptr a, Int) -> PtrLenTuple as -> PtrLenTuple (a : as)

instance PluralPtrLen (PtrLenTuple '[]) where
	type ValueLists (PtrLenTuple '[]) = ListTuple '[]
	toIovecList _ = []
	allocaPluralPtrLen _ = ($ PtrLenTupleNil)
	peekPluralPtrLen _ = return ListTupleNil
	pokePluralPtrLen _ _ = return ()
	valueListLengths _ = []

instance (Storable a, PluralPtrLen (PtrLenTuple as),
			ValueLists (PtrLenTuple as) ~ ListTuple as) =>
		PluralPtrLen (PtrLenTuple (a : as)) where
	type ValueLists (PtrLenTuple (a : as)) = ListTuple (a : as)
	toIovecList ((p, n) :-- pns) =
		Iovec (castPtr p) (fromIntegral $ n * sizeOfPtr p) :
		toIovecList pns
	allocaPluralPtrLen [] _ = errorWithFunName
		("instance PluralPtrLen (PtrLenTuple (a : as)): " ++ 
			"allocaPluralPtrLen")
		"need more element lengths"
	allocaPluralPtrLen (n : ns) act =
		allocaArray n $ \p -> allocaPluralPtrLen ns $ act . ((p, n) :--)
	peekPluralPtrLen ((p, n) :-- pns) =
		(:.) <$> peekArray n p <*> peekPluralPtrLen pns
	pokePluralPtrLen ((p, _n) :-- pns) (v :. vs) =
		pokeArray p v >> pokePluralPtrLen pns vs
	valueListLengths (vl :. vls) = length vl : valueListLengths vls

data ListTuple :: [*] -> * where
	ListTupleNil :: ListTuple '[]
	(:.) :: [a] -> ListTuple as -> ListTuple (a : as)

byteLengthPluralPtrLen :: PluralPtrLen ppl => ppl -> Int
byteLengthPluralPtrLen = sum . ((\(Iovec _ l) -> fromIntegral l) <$>) . toIovecList

peekByteStringPluralPtrLen :: PluralPtrLen ppl => ppl -> Int -> IO [BS.ByteString]
peekByteStringPluralPtrLen ppl n =
	mapM BS.packCStringLen . takeCCharPtrLenList n $ (\(Iovec p l) -> (p, fromIntegral l)) <$> toIovecList ppl

takeCCharPtrLenList :: Int -> [(Ptr CChar, Int)] -> [(Ptr CChar, Int)]
takeCCharPtrLenList _ [] = error "takeCCharPtrLenList: n should be less than sum of length"
takeCCharPtrLenList n _ | n < 0 = error $ "takeCCharPtrLenList: n should be positive integer: " ++ show n
takeCCharPtrLenList 0 _ = []
takeCCharPtrLenList n ((pc, l) : pcls)
	| n <= l = [(pc, n)]
	| otherwise = (pc, l) : takeCCharPtrLenList (n - l) pcls
