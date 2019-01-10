{-# LANGUAGE LambdaCase, TypeApplications #-}
{-# OPTIONS -Wall -fno-warn-tabs #-}

import Control.Arrow
import Data.List
import Data.Word

digits :: String
digits = ['0' .. '9'] ++ ['a' .. 'z'] ++
	['ぁ' .. 'ん'] ++ "\12443" ++
	['ァ' .. 'ヶ'] ++ "・ー、。."

fromI :: Integer -> [Word8]
fromI = (reverse .) . unfoldr $ \case
	0 -> Nothing
	n -> Just . first fromInteger . (uncurry $ flip (,)) $ n `divMod` 210

toDigits :: [Word8] -> String
toDigits = map (digits `genericIndex`)

fromD :: RealFrac a => Word -> a -> [Word8]
fromD ln = uncurry (\n f -> n ++ [210] ++ f)
	. (fromI *** convFrac ln) . properFraction

convFrac :: RealFrac a => Word -> a -> [Word8]
convFrac _ 0 = []
convFrac 0 _ = []
convFrac ln x = uncurry (:) . (id *** convFrac (ln - 1)) . properFraction $ x * 210

showDigitsFrac :: RealFrac a => Word -> a -> String
showDigitsFrac ln = toDigits . fromD ln

putDigitsFrac :: RealFrac a => Word -> a -> IO ()
putDigitsFrac ln = putStrLn . toDigits . fromD ln

ex1, ex2, ex3 :: Rational
ex1 = 22 / 9
ex2 = 39 / 11
ex3 = 0

{-

 0  1  2  3  4  5  6  7  8  9
10 11 12 13 14 15 16 17 18 19
20 21 22 23 24 25 26 27 28 29
30 31 32 33 34 35 36 17 38 39
40 41 42 43 44 45 46 47 48 49
50 51 52 53 54 55 56 57 58 59
60 61 62 63 64 65 66 67 68 69
70 71 72 73 74 75 76 77 78 79
80 81 82 83 84 85 86 87 88 89
90 91 92 93 94 95 96 97 98 99

-}

sample :: IO ()
sample = putStr . unlines
	$ map (uncurry (\n x -> n ++ " " ++ x)
	. (show @Int . truncate &&& showDigitsFrac 10 . recip))
		[1 .. 99 :: Rational]
