{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MyWords where

import Control.Arrow
import Data.Bits
import Data.Word

newtype Word5 = Word5 { unWord5 :: Word8 } deriving (Show, Eq)

instance Bits Word5 where
	(.&.) = op Word5 unWord5 (.&.)
	(.|.) = op Word5 unWord5 (.|.)
	xor = op Word5 unWord5 xor
	complement = fun (Word5 . (.&. 0x1f)) unWord5 complement
	shift = flip $ fun (Word5 . (.&. 0x1f)) unWord5 . flip shift
	rotateL = myRotateL; rotateR = myRotateR
	bitSize _ = 5; bitSizeMaybe _ = Just 5
	isSigned _ = False
	testBit = testBit . unWord5
	bit = Word5 . (.&. 0x1f) . bit
	popCount = popCount . unWord5

instance FiniteBits Word5 where finiteBitSize _ = 5

instance Num Word5 where
	(+) = op Word5 unWord5 (+)
	(*) = op Word5 unWord5 (*)
	abs = fun Word5 unWord5 abs
	signum = fun Word5 unWord5 signum
	fromInteger = Word5 . fromInteger
	negate = fun Word5 unWord5 negate

instance Ord Word5 where w1 <= w2 = unWord5 w1 <= unWord5 w2

instance Enum Word5 where toEnum = Word5 . toEnum; fromEnum = fromEnum . unWord5

instance Real Word5 where toRational = toRational . unWord5

instance Integral Word5 where
	Word5 w1 `quotRem` Word5 w2 = Word5 *** Word5 $ w1 `quotRem` w2
	toInteger = toInteger . unWord5

newtype Word30 = Word30 { unWord30 :: Word32 } deriving (Show, Eq)

word30ToWord5List :: Word30 -> [Word5]
word30ToWord5List w30 = one <$> [5, 4 .. 0]
	where
	one i = fromIntegral
		$ (w30 .&. bits [i * 5, i * 5 + 1 .. i * 5 + 4]) `shiftR` (i * 5)

instance Bits Word30 where
	(.&.) = op Word30 unWord30 (.&.)
	(.|.) = op Word30 unWord30 (.|.)
	xor = op Word30 unWord30 xor
	complement = fun (Word30 . (.&. 0x3fffffff)) unWord30 complement
	shift = flip $ fun (Word30 . (.&. 0x3fffffff)) unWord30 . flip shift
	rotateL = myRotateL; rotateR = myRotateR
	bitSize _ = 30; bitSizeMaybe _ = Just 30
	isSigned _ = False
	testBit = testBit . unWord30
	bit = Word30 . (.&. 0x3fffffff) . bit
	popCount = popCount . unWord30

instance FiniteBits Word30 where
	finiteBitSize _ = 30

instance Num Word30 where
	(+) = op Word30 unWord30 (+)
	(*) = op Word30 unWord30 (*)
	abs = fun Word30 unWord30 abs
	signum = fun Word30 unWord30 signum
	fromInteger = Word30 . fromInteger
	negate = fun Word30 unWord30 negate

instance Ord Word30 where w1 <= w2 = unWord30 w1 <= unWord30 w2

instance Enum Word30 where
	toEnum = Word30 . toEnum; fromEnum = fromEnum . unWord30

instance Real Word30 where toRational = toRational . unWord30

instance Integral Word30 where
	Word30 w1 `quotRem` Word30 w2 = Word30 *** Word30 $ w1 `quotRem` w2
	toInteger = toInteger . unWord30

newtype Word40 = Word40 { unWord40 :: Word64 } deriving (Show, Eq, Ord)

instance Bits Word40 where
	(.&.) = op Word40 unWord40 (.&.)
	(.|.) = op Word40 unWord40 (.|.)
	xor = op Word40 unWord40 xor
	complement = fun Word40 unWord40 complement
	shift = flip $ fun (Word40 . (.&. 0xffffffffff)) unWord40 . flip shift
	rotateL = myRotateL; rotateR = myRotateR
	bitSize _ = 40; bitSizeMaybe _ = Just 40; isSigned _ = False
	testBit = testBit . unWord40
	bit = Word40 . bit
	popCount = popCount . unWord40

instance FiniteBits Word40 where finiteBitSize _ = 40

instance Enum Word40 where
	toEnum = Word40 . toEnum; fromEnum = fromEnum . unWord40

instance Num Word40 where
	(+) = op Word40 unWord40 (+)
	(*) = op Word40 unWord40 (*)
	abs = fun Word40 unWord40 abs
	signum = fun Word40 unWord40 signum
	fromInteger = Word40 . fromInteger
	negate = fun Word40 unWord40 negate

instance Real Word40 where toRational = toRational . unWord40

instance Integral Word40 where
	Word40 w1 `quotRem` Word40 w2 = Word40 *** Word40 $ w1 `quotRem` w2
	toInteger = toInteger . unWord40

-- word5sToWord40s :: [Word5] -> ([Word40], Int)
-- word5sToWord40s =

each :: Int -> [a] -> ([[a]], Int)
each _ [] = ([], 0)
each n (x : xs) = go (n - 1) [x] xs
	where
	go i s [] = ([reverse s], i)
	go i s xa@(x : xs)
		| i < 1 = (reverse s :) `first` go (n - 1) [x] xs
		| otherwise = go (i - 1) (x : s) xs

pushToHead :: a -> [[a]] -> [[a]]
pushToHead x = \case [] -> [[x]]; xs : xss -> (x : xs) : xss

fun :: (w0 -> w1) -> (w1 -> w0) -> (w0 -> w0) -> w1 -> w1
fun w unw f w1 = w . f $ unw w1

op :: (w0 -> w1) -> (w1 -> w0) -> (w0 -> w0 -> w0) -> w1 -> w1 -> w1
op w unw o w1 w2 = w $ unw w1 `o` unw w2

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
