{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module SafeWord (SafeWord(Err)) where

import Control.Arrow
import Data.Bits

data SafeWord = Err | SafeWord Word

instance Show SafeWord where
	show (SafeWord m) = show m
	show Err = "Err"

instance Eq SafeWord where
	SafeWord m == SafeWord n = m == n
	_ == _ = error "Err"

instance Num SafeWord where
	SafeWord m + SafeWord n
		| mn < m || mn < n = Err
		| otherwise = SafeWord mn
		where mn = m + n
	_ + _ = Err
	SafeWord m - SafeWord n
		| m < n = Err
		| otherwise = SafeWord $ m - n
	_ - _ = Err
	Err * _ = Err
	_ * Err = Err
	_ * 0 = 0
	m * 1 = m
	m * n	| not (n `testBit` 0) && not (m `testBit` finiteBitSize m) =
			(m `shiftL` 1) * (n `shiftR` 1)
		| not (m `testBit` finiteBitSize m) = m * (n .&. complement 1) + m
		| otherwise = Err
	negate z@(SafeWord 0) = z
	negate _ = Err
	abs = id
	signum z@(SafeWord 0) = z
	signum (SafeWord _) = 1
	signum _ = Err
	fromInteger n
		| n < 0 || n > fromIntegral (maxBound :: Word) = Err
		| otherwise = SafeWord $ fromInteger n

instance Integral SafeWord where
	SafeWord m `quotRem` SafeWord n = SafeWord *** SafeWord $ m `quotRem` n
	_ `quotRem` _ = (Err, Err)
	toInteger Err = - 1
	toInteger (SafeWord m) = toInteger m

instance Ord SafeWord where
	SafeWord m <= SafeWord n = m <= n
	_ <= _ = error "Err"

instance Real SafeWord where
	toRational (SafeWord m) = toRational m
	toRational Err = error "Err"

instance Enum SafeWord where
	toEnum n
		| n < 0 = Err
		| otherwise = SafeWord $ toEnum n
	fromEnum Err = - 1
	fromEnum (SafeWord m) = fromEnum m

instance Bits SafeWord where
	SafeWord m .&. SafeWord n = SafeWord $ m .&. n
	_ .&. _ = Err
	SafeWord m .|. SafeWord n = SafeWord $ m .|. n
	_ .|. _ = Err
	SafeWord m `xor` SafeWord n = SafeWord $ m `xor` n
	_ `xor` _ = Err
	complement (SafeWord m) = SafeWord $ complement m
	complement Err = Err
	SafeWord m `shift` i = SafeWord $ m `shift` i
	Err `shift` _ = Err
	SafeWord m `rotate` i = SafeWord $ m `rotate` i
	Err `rotate` _ = Err
	bitSizeMaybe ~(SafeWord n) = bitSizeMaybe n
	bitSize ~(SafeWord n) = bitSize n
	isSigned ~(SafeWord n) = isSigned n
	SafeWord m `testBit` i = m `testBit` i
	Err `testBit` _ = error "Err"
	bit i = SafeWord $ bit i
	popCount (SafeWord n) = popCount n
	popCount Err = error "Err"

instance FiniteBits SafeWord where
	finiteBitSize ~(SafeWord m) = finiteBitSize m
