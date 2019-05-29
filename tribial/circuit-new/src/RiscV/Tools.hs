module RiscV.Tools where

import Data.Bits
import Data.Bool
import Data.Word
import Data.Int

import Circuit

import qualified Data.Bits as B

numToBits :: Bits n => Word8 -> n -> [Bit]
numToBits n _ | n > 64 = error "numToBits bits integer : bits >= 0 && bits <= 64"
numToBits 0 _ = []
numToBits n i = bool O I (i `testBit` 0) : numToBits (n - 1) (i `shiftR` 1)

bitsToNum :: Bits n => [Bit] -> n
bitsToNum [] = zeroBits
bitsToNum (O : bs) = bitsToNum bs `shiftL` 1
bitsToNum (I : bs) = bitsToNum bs `shiftL` 1 .|. zeroBits `B.setBit` 0
