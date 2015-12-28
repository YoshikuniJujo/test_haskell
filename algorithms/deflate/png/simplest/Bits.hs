{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Bits (
	Bits, complement, (.&.), (.|.), xor, shiftL, shiftR, testBit,
	popBit, popBits, popByte,
	beToByteString, beFromByteString,
	leToByteString) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((&&&), (***))
import Data.Bits (Bits, complement, testBit, (.&.), (.|.), xor, shiftL, shiftR)
import Data.Word (Word8)

import qualified Data.ByteString as BS (ByteString, cons, foldl')

beToByteString :: (Integral a, Bits a) => Int -> a -> BS.ByteString
beToByteString = btb "" where
	btb s 0 _ = s
	btb s c n = btb (BS.cons (fromIntegral n) s) (c - 1) (n `shiftR` 8)

beFromByteString :: (Num a, Bits a) => BS.ByteString -> a
beFromByteString =
	BS.foldl' (curry $ uncurry (.|.) . ((`shiftL` 8) *** fromIntegral)) 0

leToByteString :: (Integral a, Bits a) => Int -> a -> BS.ByteString
leToByteString 0 = const ""
leToByteString c =
	BS.cons <$> fromIntegral <*> leToByteString (c - 1) . (`shiftR` 8)

popByte :: (Integral a, Bits a) => a -> (Word8, a)
popByte = fromIntegral &&& (`shiftR` 8)

popBits :: Bits n => n -> Int -> ([Bool], n)
popBits n c = (<$> [0 .. c - 1]) . testBit &&& (`shiftR` c) $ n

popBit :: Bits a => a -> (Bool, a)
popBit = (`testBit` 0) &&& (`shiftR` 1)
