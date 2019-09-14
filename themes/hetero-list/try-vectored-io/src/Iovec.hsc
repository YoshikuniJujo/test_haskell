{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Iovec (
	-- * Iovec
	Iovec, withIovec,

	-- * PluralArray
	PluralArray, ValueLists, allocaPluralArray,
	peekPluralArray, peekByteStringPluralArray, pluralArrayByteLength,
	pokePluralArray, valueListLengthList,

	-- * Instances of PluralArray
	ArrayList(..), ArrayTuple(..), ListTuple(..) ) where

import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (Storable(..), pokeByteOff)
import Foreign.Marshal (allocaBytes)
import Foreign.C.Types (CSize, CChar, CInt)
import Control.Arrow ((***))
import Data.List (genericLength)

import Iovec.PluralArray (
	Iovec(..), PluralArray(..),
	peekByteStringPluralArray, pluralArrayByteLength,
	ArrayList(..), ArrayTuple(..), ListTuple(..) )
import Tools (for2M_)

withIovec :: PluralArray ppl => ppl -> (Ptr Iovec -> CInt -> IO a) -> IO a
withIovec = c_withIovec . ((\(Iovec p n) -> (p, n)) <$>) . toIovecList

#include <sys/uio.h>

c_withIovec :: [(Ptr CChar, CSize)] -> (Ptr Iovec -> CInt -> IO a) -> IO a
c_withIovec pns act = allocaBytes (ln * #{size struct iovec}) $ \piov ->
	c_prepareIovec piov pns >> act piov ln
	where ln :: Num n => n; ln = genericLength pns

c_prepareIovec :: Ptr Iovec -> [(Ptr CChar, CSize)] -> IO ()
c_prepareIovec piov0 pns = for2M_ is pns $ \i -> uncurry (>>)
	. (#{poke struct iovec, iov_base} i *** #{poke struct iovec, iov_len} i)
	where is = (`plusPtr` #{size struct iovec}) `iterate` piov0
