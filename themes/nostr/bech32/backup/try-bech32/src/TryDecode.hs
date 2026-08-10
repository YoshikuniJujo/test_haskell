{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryDecode where

import Control.Arrow
import Data.Bits
import Data.Maybe
import Data.List qualified as L
import Data.Word
import Data.Text qualified as T

import Examples

nip49afterksb :: [Word8]
nip49afterksb = tail nip49afternonce

nip49ksb :: Word8
nip49ksb = head nip49afternonce

nip49afternonce :: [Word8]
nip49afternonce = drop 24 nip49aftersalt

nip49nonce :: [Word8]
nip49nonce = take 24 nip49aftersalt

nip49aftersalt :: [Word8]
nip49aftersalt = drop 18 nip49w8s

nip49salt :: [Word8]
nip49salt = take 16 $ drop 2 nip49w8s

nip49w8s :: [Word8]
nip49w8s = concat $ reverse . bytes5ToBytes 5 <$> nip49b5s

nip49b5s :: [Bytes5]
nip49b5s = foldl (\b w -> w .|. (b `shiftL` 5)) 0 <$> nip49ws'

nip49ws, nip49ws' :: [[Bytes5]]
nip49ws = map (map dict . T.unpack) nip49dp3
nip49ws' = (\ws -> ws ++ replicate (8 - length ws) 0) <$> nip49ws

nip49dp3 :: [T.Text]
nip49dp3 = splits 8 nip49dp'

nip49dp', nip49cs :: T.Text
(nip49dp', nip49cs) = T.splitAt (T.length nip49dp - 6) nip49dp

nip49hrp, nip49dp :: T.Text
(nip49hrp, nip49dp) = separate nip49

separate :: T.Text -> (T.Text, T.Text)
separate = spanEnd (/= '1')

data Identity a = Identity { getIdentity :: a } deriving Show

instance Functor Identity where
	f `fmap` Identity x = Identity $ f x

instance Applicative Identity where
	pure = Identity
	Identity f <*> Identity x = Identity $ f x

instance Monad Identity where
	Identity x >>= f = f x

spanEnd :: (Char -> Bool) -> T.Text -> (T.Text, T.Text)
spanEnd f = getIdentity . T.spanEndM (Identity . f)

splits :: Int -> T.Text -> [T.Text]
splits _ "" = []
splits n txt = let (t, d) = T.splitAt n txt in t : splits n d

data Bytes5 = Bytes5 Word64 deriving (Show, Eq)

bytes5ToBytes :: Int -> Bytes5 -> [Word8]
bytes5ToBytes n _ | n < 1 = []
bytes5ToBytes n b = fromIntegral (b .&. 0xff) : bytes5ToBytes (n - 1) (b `shiftR` 8)

dictChars :: [Char]
dictChars = "qpzry9x8gf2tvdw0s3jn54khce6mua7l"

dict :: Char -> Bytes5
dict = fromIntegral . fromJust . (`L.elemIndex` dictChars)

instance Bits Bytes5 where
	Bytes5 b1 .&. Bytes5 b2 = Bytes5 $ b1 .&. b2
	Bytes5 b1 .|. Bytes5 b2 = Bytes5 $ b1 .|. b2
	Bytes5 b1 `xor` Bytes5 b2 = Bytes5 $ b1 `xor` b2
	complement (Bytes5 b) = Bytes5 $ b `xor` 0xffffffffff
	shift (Bytes5 b) n = Bytes5 $ shift b n .&. 0xffffffffff
	rotateL = myRotateL 40; rotateR = myRotateR 40
	bitSize _ = 40; bitSizeMaybe _ = Just 40
	isSigned _ = False
	bit n = Bytes5 $ bit n
	popCount (Bytes5 b) = popCount b
	testBit (Bytes5 b) n = testBit b n

instance Num Bytes5 where
	Bytes5 b1 + Bytes5 b2 = Bytes5 $ (b1 + b2) .&. 0xffffffffff
	Bytes5 b1 * Bytes5 b2 = Bytes5 $ (b1 * b2) .&. 0xffffffffff
	abs (Bytes5 b) = Bytes5 $ abs b
	signum (Bytes5 b) = Bytes5 $ signum b
	fromInteger n = Bytes5 $ fromInteger n .&. 0xffffffffff
	negate (Bytes5 b) = Bytes5 $ negate b

instance Ord Bytes5 where
	Bytes5 b1 <= Bytes5 b2 = b1 <= b2

instance Enum Bytes5 where
	toEnum = Bytes5 . toEnum
	fromEnum (Bytes5 n) = fromEnum n

instance Real Bytes5 where
	toRational (Bytes5 n) = toRational n

instance Integral Bytes5 where
	Bytes5 b1 `quotRem` Bytes5 b2 = Bytes5 *** Bytes5 $ b1 `quotRem` b2
	toInteger (Bytes5 b) = toInteger b

bits :: Bits a => [Int] -> a
bits = foldl setBit zeroBits

showBits :: Bits a => a -> String
showBits bs
	| bs == zeroBits = "0b"
	| otherwise = showBits (bs `shiftR` 1) ++ if testBit bs 0 then "1" else "0"

separateBits :: Bits a => a -> Int -> (a, a)
separateBits bs n = (bs .&. bits [n .. 39], bs .&. bits [0 .. n - 1])

myRotateL :: Bits a => Int -> a -> Int -> a
myRotateL m bs n = let (l, r) = separateBits bs (m - n) in
	r `shiftL` n .|. l `shiftR` (m - n)

myRotateR :: Bits a => Int -> a -> Int -> a
myRotateR m bs n = let (l, r) = separateBits bs n in
	r `shiftL` (m - n) .|. l `shiftR` n
