{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module BitsAndWord32 (bitsToWord32, word32ToBits) where

import Language.Haskell.TH
import Data.List.Length
import Data.Bits
import Data.Bool
import Data.Word

import CheckEndian

data Bit = O | I deriving (Show, Enum)

bitsToWord32 :: LengthL 32 Bit -> Word32
word32ToBits :: Word32 -> LengthL 32 Bit
(bitsToWord32, word32ToBits) = $(do
	e <- runIO targetEndian
	case e of
		Right LittleEndian -> tupE [
			varE $ mkName "bitsToWord32LittleE",
			varE $ mkName "word32ToBitsLittleE" ]
		Right BigEndian -> tupE [
			varE $ mkName "bitsToWord32BigE'",
			varE $ mkName "word32ToBitsBigE'" ]
		Right UnknownEndian -> fail "unknown endian"
		Left msg -> fail msg)

bitsToWord32LittleE :: LengthL 32 Bit -> Word32
bitsToWord32LittleE = foldr (\b w -> w `shift` 1 .|. fromIntegral (fromEnum b)) 0

exampleBits :: LengthL 32 Bit
exampleBits =
	(repeatL O :: LengthL 8 Bit) ++. (repeatL I :: LengthL 8 Bit) ++.
	(repeatL O :: LengthL 8 Bit) ++. (repeatL I :: LengthL 8 Bit)

word32ToBitsLittleE :: Word32 -> LengthL 32 Bit
word32ToBitsLittleE = unfoldr \w -> (bool O I (w `testBit` 0), w `shiftR` 1)

bitsToWord32BigE :: LengthR 32 Bit -> Word32
bitsToWord32BigE = foldl (\w b -> w `shift` 1 .|. fromIntegral (fromEnum b)) 0

bitsToWord32BigE' :: LengthL 32 Bit -> Word32
bitsToWord32BigE' = bitsToWord32BigE . leftToRight

word32ToBitsBigE :: Word32 -> LengthR 32 Bit
word32ToBitsBigE = unfoldl \w -> (bool O I (w `testBit` 0), w `shiftR` 1)

word32ToBitsBigE' :: Word32 -> LengthL 32 Bit
word32ToBitsBigE' = rightToLeft . word32ToBitsBigE
