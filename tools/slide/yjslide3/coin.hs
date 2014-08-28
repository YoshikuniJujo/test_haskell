
import Control.Applicative
import System.Random
import Data.Word

coin :: [Bool]
coin = map (> (63 :: Word8)) . randoms $ mkStdGen 4492

sep2 :: [a] -> [(a, a)]
sep2 xs = map snd . filter fst .
	zip (cycle [True, False]) . zip xs $ tail xs

avrg :: Eq a => [(a, a)] -> [a]
avrg = map fst . filter (uncurry (/=))

fun :: [Bool] -> [Bool]
fun = avrg . sep2

odds :: [a] -> [a]
odds = map snd . filter fst . zip (cycle [True, False])

f :: [a] -> [(a, a)]
f = ($) <$> zip <*> tail

sep2' = map snd . filter fst .
	zip (cycle [True, False]) . (($) <$> zip <*> tail)

fun' :: Eq a => [a] -> [a]
fun' = map fst . filter (uncurry (/=)) . map snd . filter fst .
		zip (cycle [True, False]) . (($) <$> zip <*> tail)
