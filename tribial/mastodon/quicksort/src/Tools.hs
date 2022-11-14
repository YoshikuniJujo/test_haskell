{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tools where

import Control.Monad
import Data.Word
import System.IO
import System.Random

import Data.Time

i3, i4, i5, i6, i7, i8, i9,
	i10, i14, i16, i17, i18, i19, i20, i21, i22, i23 :: Int
i3 = 3; i4 = 4; i5 = 5; i6 = 6; i7 = 7; i8 = 8; i9 = 9; i10 = 10
i14 = 14; i16 = 16; i17 = 17; i18 = 18; i19 = 19; i20 = 20
i21 = 21; i22 = 22; i23 = 23

showTime :: String -> Int -> IO a -> IO ()
showTime nm n act = do
	putStr $ nm ++ ":\t"
	t <- time act
	print' t
	print $ realToFrac t / nLogN n * 10 ^ i7

showTimeMGraph :: Handle -> Int -> IO a -> IO ()
showTimeMGraph h m act = do
	hPutStr h $ show m ++ "\t"
	t <- time act
	hPutStrLn h $ show t

print' :: Show a => a -> IO ()
print' x = putStr $ show x ++ "\t"

time :: IO a -> IO NominalDiffTime
time act = do
	t0 <- getCurrentTime
	void act
	flip diffUTCTime t0 <$> getCurrentTime

nLogN :: Int -> Double
nLogN (fromIntegral -> n) = n * log n

mkSample :: (Int, Int) -> IO [Int]
mkSample r = do
	g <- newStdGen
	let	(n, g') = randomR (0, 200) g
	pure . take n $ randomRs r g'

mkSample'' :: (Word32, Word32) -> Int -> IO [Word32]
mkSample'' r n = do
	g <- newStdGen
	pure . take n $ randomRs r g

mkSample' :: (Int, Int) -> Int -> IO [Int]
mkSample' r n = do
	g <- newStdGen
	pure . take n $ randomRs r g

checkSample :: Ord a => [a] -> Bool
checkSample = \case
	[] -> True
	[_] -> True
	x : xs@(y : _)
		| x <= y -> checkSample xs
		| otherwise -> False
