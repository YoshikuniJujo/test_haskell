module B where

import Data.Bits

data B = B [Bool] deriving Eq

instance Bits B where
	B a .&. B b = B $ zipWith (.&.) a b
	B a .|. B b = B $ zipWith (.|.) a b
	B a `xor` B b = B $ zipWith xor a b
	complement (B a) = B $ map complement a
	x@(B a) `shiftL` i
		| i < 0 = x `shiftR` (- i)
		| otherwise = B $ replicate i False ++ a
	x@(B a) `shiftR` i
		| i < 0 = x `shiftL` (- i)
		| otherwise = B $ drop i a
	x@(B a) `rotateL` i
		| i < 0 = x `rotateR` (- i)
		| otherwise = let a' = reverse a in
			B . reverse $ drop i a' ++ take i a'
	x@(B a) `rotateR` i
		| i < 0 = x `rotateL` (- i)
		| otherwise = B $ drop i a ++ take i a
	bitSizeMaybe _ = Nothing
	bitSize _ = error "No size"
	isSigned _ = False
	x@(B a) `testBit` i = a !! i
	bit n = B $ replicate n False ++ [True]
	popCount (B a) = length $ filter id a

instance Num

{-
instance Num B where
	fromInteger = B . fi
		where
		fi n | n < 1 = []
		fi n = (n `mod` 2 == 1) : fi (n `div` 2)
	B a + B b = B $ ad a b
		where
		ad xs [] = xs
		ad [] ys = ys
		ad (False : xs) (y : ys) = y : ad xs ys
		ad (x : xs) (False : ys) = x : ad xs ys
		ad (_ : xs) (_ : ys) = False : 
		-}
