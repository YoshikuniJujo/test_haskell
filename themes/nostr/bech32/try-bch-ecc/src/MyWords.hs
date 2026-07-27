{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MyWords where

import Data.Bits
import Data.Word

newtype Word5 = Word5 Word8 deriving (Show, Eq)

instance Bits Word5 where
	Word5 w1 .&. Word5 w2 = Word5 $ w1 .&. w2
	Word5 w1 .|. Word5 w2 = Word5 $ w1 .|. w2
	Word5 w1 `xor` Word5 w2 = Word5 $ w1 `xor` w2
	complement (Word5 w) = Word5 $ complement w .&. 0x1f
	Word5 w `shift` i = Word5 $ (w `shift` i) .&. 0x1f
	rotateL = myRotateL; rotateR = myRotateR
	bitSize _ = 5; bitSizeMaybe _ = Just 5
	isSigned _ = False
	Word5 w `testBit` i = w `testBit` i
	bit i = Word5 $ bit i .&. 0x1f
	popCount (Word5 w) = popCount w

instance FiniteBits Word5 where
	finiteBitSize _ = 5

newtype Word30 = Word30 Word32 deriving Show

bits :: Bits bs => [Int] -> bs
bits = foldl setBit zeroBits

bSplitAtR :: Bits bs => bs -> Int -> (bs, bs)
bSplitAtR bs i = (bs .&. complement mask, bs .&. mask)
	where mask = bits [0 .. i - 1]

bSplitAtL :: FiniteBits bs => bs -> Int -> (bs, bs)
bSplitAtL bs i = (bs .&. mask, bs .&. complement mask)
	where
	n = finiteBitSize bs
	mask = bits [n, n - 1 .. n - i]

myRotateR :: FiniteBits w => w -> Int -> w
myRotateR w i = b `shiftR` i .|. l `shiftL` (finiteBitSize w - i)
	where
	(b, l) = bSplitAtR w i

myRotateL :: FiniteBits w => w -> Int -> w
myRotateL w i = b `shiftR` (finiteBitSize w - i) .|. l `shiftL` i
	where
	(b, l) = bSplitAtL w i
