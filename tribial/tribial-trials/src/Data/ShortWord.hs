{-# LANGUAGE BinaryLiterals #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.ShortWord (Word3, Word6) where

import Control.Arrow
import Data.Bits
import Data.Word

data Word3 = Word3 Word8 deriving (Eq, Ord)

instance Num Word3 where
	Word3 n + Word3 m = Word3 $ (n + m) .&. 0b111
	Word3 n * Word3 m = Word3 $ (n * m) .&. 0b111
	abs (Word3 n) = Word3 $ abs n
	signum (Word3 n) = Word3 $ signum n
	fromInteger = Word3 . (.&. 0b111) . fromInteger
	negate (Word3 n) = Word3 $ negate n

instance Enum Word3 where
	toEnum = Word3 . (.&. 0b111) . toEnum
	fromEnum (Word3 n) = fromEnum n

instance Real Word3 where toRational (Word3 n) = toRational n

instance Integral Word3 where
	Word3 n `quotRem` Word3 m = Word3 *** Word3 $ n `quotRem` m
	toInteger (Word3 n) = toInteger n

instance Show Word3 where show (Word3 n) = show n

data Word6 = Word6 Word8 deriving (Eq, Ord)

instance Num Word6 where
	Word6 n + Word6 m = Word6 $ (n + m) .&. 0b111111
	Word6 n * Word6 m = Word6 $ (n * m) .&. 0b111111
	abs (Word6 n) = Word6 $ abs n
	signum (Word6 n) = Word6 $ signum n
	fromInteger = Word6 . (.&. 0b111111) . fromInteger
	negate (Word6 n) = Word6 $ negate n

instance Enum Word6 where
	toEnum = Word6 . (.&. 0b111111) . toEnum
	fromEnum (Word6 n) = fromEnum n

instance Real Word6 where toRational (Word6 n) = toRational n

instance Integral Word6 where
	Word6 n `quotRem` Word6 m = Word6 *** Word6 $ n `quotRem` m
	toInteger (Word6 n) = toInteger n

instance Bits Word6 where
	Word6 n .&. Word6 m = Word6 $ n .&. m
	Word6 n .|. Word6 m = Word6 $ n .|. m
	Word6 n `xor` Word6 m = Word6 $ n `xor` m
	complement (Word6 n) = Word6 $ complement n
	Word6 n `shift` d = Word6 $ n `shift` d
	Word6 n `rotate` d = Word6 $ n `rotate` d
	bitSize _ = 6
	bitSizeMaybe _ = Just 6
	isSigned _ = False
	Word6 n `testBit` i = n `testBit` i
	bit i = Word6 $ bit i
	popCount (Word6 n) = popCount n

instance Show Word6 where show (Word6 n) = show n
