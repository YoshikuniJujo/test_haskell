{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Pair(Pairable(..), TPair, WPair, Word64, Word32) where

import Data.Word
import Data.Bits

newtype TPair a = TPair (a, a)
newtype WPair a = WPair Word64

class Pairable p e where
	pair :: e -> e -> p e
	unpair :: p e -> (e, e)

instance Pairable TPair e where
	pair x y = TPair (x, y)
	unpair (TPair p) = p

instance Pairable WPair Word8 where
	pair w1 w2 = WPair $ (fromIntegral w1 `shiftL` 8) .|. fromIntegral w2
	unpair (WPair w) = (fromIntegral $ w `shiftR` 8, fromIntegral $ w .&. 0xff)

instance Pairable WPair Word16 where
	pair w1 w2 = WPair $ (fromIntegral w1 `shiftL` 16) .|. fromIntegral w2
	unpair (WPair w) = (fromIntegral $ w `shiftR` 16, fromIntegral $ w .&. 0xffff)

instance Pairable WPair Word32 where
	pair w1 w2 = WPair $ (fromIntegral w1 `shiftL` 32) .|. fromIntegral w2
	unpair (WPair w) = (fromIntegral $ w `shiftR` 32, fromIntegral $ w .&. 0xffffffff)
