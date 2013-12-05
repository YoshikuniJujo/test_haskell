{-# LANGUAGE OverloadedStrings #-}

import Data.Char
import Data.Word
import Data.Bits
import Data.String

toWord8s :: Integer -> [Word8]
toWord8s 0 = []
toWord8s i = fromInteger (i .&. 0xFF) : toWord8s (i `shiftR` 8)

fromWord8s :: [Word8] -> Integer
fromWord8s [] = 0
fromWord8s (w : ws) = toInteger w .|. (fromWord8s ws `shiftL` 8)

word8ToChar :: Word8 -> Char
word8ToChar = chr . fromIntegral

charToWord8 :: Char -> Word8
charToWord8 = fromIntegral . ord

toString :: Integer -> String
toString = map word8ToChar . toWord8s

instance IsString Integer where
	fromString = fromWord8s . map charToWord8
