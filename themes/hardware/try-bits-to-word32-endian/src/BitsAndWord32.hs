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
