{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Arrow
import Data.List qualified as L
import System.Environment
import Prime

main :: IO ()
main = do
	ln : _ <- (read <$>) <$> getArgs
	putStr . unlines
		$ (\(n, m) -> show n ++ "\t" ++ show m) <$> take ln amicables

pairs :: Int -> [(Int, (Int, Int))]
pairs n = (id &&& (n `divMod`)) <$> takeWhile ((n >=) . (^ (2 :: Int))) primes

popPrime :: Int -> Maybe (Int, Int)
popPrime 1 = Nothing
popPrime n = case filter ((== 0) . snd . snd) $ pairs n of
	[] -> Just (n, 1)
	(p, (n', _)) : _ -> Just (p, n')

primeFactorization :: Int -> [Int]
primeFactorization = L.unfoldr popPrime

divisors :: Int -> [Int]
divisors = L.nub . L.sort . init
	. (product <$>) . L.subsequences . primeFactorization

testAmicables :: Int -> Bool
testAmicables n = let m = sum (divisors n) in n == sum (divisors m)

amicables :: [(Int, Int)]
amicables = filter ((<) <$> fst <*> snd)
	[ (n, sum $ divisors n) | n <- [2 ..], testAmicables n ]
