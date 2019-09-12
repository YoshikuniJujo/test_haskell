{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Iovec (
	-- * Iovec
	Iovec, withIovec,

	-- * PluralPtrLen
	PluralPtrLen, ValueLists, allocaPluralPtrLen,
	peekPluralPtrLen, peekByteStringPluralPtrLen, pluralPtrLenByteLength,
	pokePluralPtrLen, valueListLengthList,

	-- * Instances of PluralPtrLen
	PtrLenList(..), PtrLenTuple(..), ListTuple(..) ) where

import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (Storable(..), pokeByteOff)
import Foreign.Marshal (allocaBytes)
import Foreign.C.Types (CInt, CChar, CSize)
import Control.Arrow ((***))
import Control.Monad
import Data.List (genericLength)

import Iovec.PluralPtrLen (
	PluralPtrLen(..), PtrLenList(..), PtrLenTuple(..), ListTuple(..),
	pluralPtrLenByteLength, peekByteStringPluralPtrLen, Iovec(..) )

withIovec :: PluralPtrLen ppl => ppl -> (Ptr Iovec -> CInt -> IO a) -> IO a
withIovec = c_withIovec . ((\(Iovec p l) -> (p, l)) <$>) . toIovecList

#include <sys/uio.h>

c_withIovec :: [(Ptr CChar, CSize)] -> (Ptr Iovec -> CInt -> IO a) -> IO a
c_withIovec pns act = allocaBytes (ln * #{size struct iovec}) $ \piov ->
	c_prepareIovec piov pns >> act piov ln
	where ln :: Num n => n; ln = genericLength pns

c_prepareIovec :: Ptr Iovec -> [(Ptr CChar, CSize)] -> IO ()
c_prepareIovec piov0 pns =
	for2M_ ((`plusPtr` #{size struct iovec}) `iterate` piov0) pns $ \piov ->
		uncurry (>>)
			. (#{poke struct iovec, iov_base} piov ***
				#{poke struct iovec, iov_len} piov)

for2M_ :: Monad m => [a] -> [b] -> (a -> b -> m c) -> m ()
for2M_ xs ys act = forM_ (xs `zip` ys) $ uncurry act
