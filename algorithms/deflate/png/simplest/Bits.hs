{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Bits (
	Bits, complement, (.&.), (.|.), xor, shiftL, shiftR, testBit,
	beToByteString, beFromByteString,
	leToByteString,
	popBit, popByte) where

import Control.Arrow
import Data.List
import Data.Bits
import Data.Bool
import Data.Word

import qualified Data.ByteString as BS

beToByteString :: (Integral a, Bits a) => Int -> a -> BS.ByteString
beToByteString c = ((BS.pack . reverse) .) . flip curry c . unfoldr $ \(i, n) ->
	bool Nothing (Just . second (i - 1 ,) $ popByte n) (i > 0)

beFromByteString :: (Num a, Bits a) => BS.ByteString -> a
beFromByteString =
	BS.foldl' (curry $ uncurry (.|.) . ((`shiftL` 8) *** fromIntegral)) 0

leToByteString :: (Integral a, Bits a) => Int -> a -> BS.ByteString
leToByteString 0 _ = ""
leToByteString c n = fromIntegral n `BS.cons` leToByteString (c - 1) (n `shiftR` 8)

popBit :: Bits a => a -> (Bool, a)
popBit = (`testBit` 0) &&& (`shiftR` 1)

popByte :: (Integral a, Bits a) => a -> (Word8, a)
popByte = fromIntegral &&& (`shiftR` 8)
