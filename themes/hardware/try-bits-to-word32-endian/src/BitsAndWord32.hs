{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module BitsAndWord32 where

import Data.List.Length
import Data.Bits
import Data.Word

data Bit = O | I deriving (Show, Enum)

bitsToWord32 :: LengthL 32 Bit -> Word32
bitsToWord32 = foldr (\b w -> w `shift` 1 .|. fromIntegral (fromEnum b)) 0

exampleBits :: LengthL 32 Bit
exampleBits =
	(repeatL O :: LengthL 8 Bit) ++. (repeatL I :: LengthL 8 Bit) ++.
	(repeatL O :: LengthL 8 Bit) ++. (repeatL I :: LengthL 8 Bit)
