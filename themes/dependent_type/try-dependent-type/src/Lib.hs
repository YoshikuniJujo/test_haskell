{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Data.Bits
import Data.Word

data W = W8 | W16 | W32 | W64 deriving Show

fun :: W -> (forall n . (Bits n, Integral n) => n -> a) -> a
fun W8 f = f (1 :: Word8)
fun W16 f = f (1 :: Word16)
fun W32 f = f (1 :: Word32)
fun W64 f = f (1 :: Word64)

foo :: W -> (Word64, Word64, Word64, Word64)
foo w = fun w \n -> (
	fromIntegral n,
	fromIntegral $ n `shift` 8,
	fromIntegral $ n `shift` 16,
	fromIntegral $ n `shift` 32 )
