{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Pair (
	Pairable(..), WPair, Word64, Word32
) where

import Data.Word
import Data.Bits

class Pairable p e where
	pair :: e -> e -> p
	unpair :: p -> (e, e)

newtype WPair = WPair Word64 deriving Show

instance Pairable (e, e) e where
	pair = (,)
	unpair = id

instance Pairable WPair Bool where
	pair b1 b2 = WPair $ fromIntegral $ fromEnum b1 `shiftL` 1 .|. fromEnum b2
	unpair (WPair w) = (w .&. 2 /= 0, w .&. 1 /= 0)

instance Pairable WPair Word8 where
	pair w1 w2 = WPair $ (fromIntegral w1 `shiftL` 8) .|. fromIntegral w2
	unpair (WPair w) = (fromIntegral $ w `shiftR` 8, fromIntegral $ w .&. 0xff)

instance Pairable WPair Word16 where
	pair w1 w2 = WPair $ (fromIntegral w1 `shiftL` 16) .|. fromIntegral w2
	unpair (WPair w) = (fromIntegral $ w `shiftR` 16, fromIntegral $ w .&. 0xffff)

instance Pairable WPair Word32 where
	pair w1 w2 = WPair $ (fromIntegral w1 `shiftL` 32) .|. fromIntegral w2
	unpair (WPair w) = (fromIntegral $ w `shiftR` 32, fromIntegral $ w .&. 0xffffffff)

instance Pairable Word64 Word32 where
	pair w1 w2 = (fromIntegral w1 `shiftL` 32) .|. fromIntegral w2
	unpair w = (fromIntegral $ w `shiftR` 32, fromIntegral $ w .&. 0xffffffff)
