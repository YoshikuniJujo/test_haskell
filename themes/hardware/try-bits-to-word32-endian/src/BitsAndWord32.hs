{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module BitsAndWord32 where

import Data.List.Length
import Data.Bits
import Data.Bool
import Data.Word

data Bit = O | I deriving (Show, Enum)

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

exampleBitsR :: LengthR 32 Bit
exampleBitsR =
	(repeatR I :: LengthR 8 Bit) +++ (repeatR O :: LengthR 8 Bit) +++
	(repeatR I :: LengthR 8 Bit) +++ (repeatR O :: LengthR 8 Bit)

word32ToBitsBigE :: Word32 -> LengthR 32 Bit
word32ToBitsBigE = unfoldl \w -> (bool O I (w `testBit` 0), w `shiftR` 1)

word32ToBitsBigE' :: Word32 -> LengthL 32 Bit
word32ToBitsBigE' = rightToLeft . word32ToBitsBigE
