{-# LANGUAGE TupleSections #-}

module ReadTable (readTable', sample', dist) where

import Data.List
import Data.Char
import Control.Applicative
import System.Environment
import System.IO.Unsafe

readTable :: String -> [[Maybe Int]]
readTable = map
	(map (\s -> case s of "-" -> Nothing; _ -> Just $ read s) . words)
	. lines

sample :: [[Maybe Int]]
sample = unsafePerformIO $ readTable <$> readFile "dijkstra_sample.txt"

readOne :: String -> [Int]
readOne "" = []
readOne str = case span (/= ',') str of
	(h, ',' : t) -> read h : readOne t
	(h, "") -> [read h]

readLines :: String -> [[Int]]
readLines (c : str) | isSpace c = readLines str
readLines ('{' : str) = case span (/= '}') str of
	(h, '}' : ',' : t) -> readOne h : readLines t
	(h, '}' : _) -> [readOne h]
	_ -> error "bad"

readTable' :: String -> [[Maybe Int]]
readTable' = map (map (\n -> if n == 0 then Nothing else Just n)) . readLines

sample' :: [[Maybe Int]]
sample' = unsafePerformIO $ readTable' <$> readFile "dijkstra_sample_1.txt"

dist :: [[Maybe Int]] -> Int -> Int -> Maybe Int
dist p x y = p !! (x - 1) !! (y - 1)

ips :: [(Int, Maybe Int)]
ips = [
	(1, Just 0), (2, Nothing), (3, Nothing),
	(4, Nothing), (5, Nothing), (6, Nothing) ]

getMin :: [(Int, Maybe Int)] -> Maybe ((Int, Int), [(Int, Maybe Int)])
getMin [] = Nothing
getMin (pnt@(_, Nothing) : p) = do
	(mn, rest) <- getMin p
	return (mn, pnt : rest)
getMin (pd@(pnt, Just d) : p) = case getMin p of
	Just (pd0@(pnt0, d0), rest) -> return $ if d <= d0
		then ((pnt, d), (pnt0, Just d0) : rest)
		else (pd0, pd : rest)
	_ -> return ((pnt, d), p)

next :: [[Maybe Int]] -> [(Int, Maybe Int)] ->
	Maybe ((Int, Int), [(Int, Maybe Int)])
next p ps = do
	(pd0@(pnt0, d0), ps') <- getMin ps
	return $ (pd0, map (calc p pd0) ps')

calc :: [[Maybe Int]] -> (Int, Int) -> (Int, Maybe Int) -> (Int, Maybe Int)
calc p (s, d0) (pnt, d) = (pnt, sel d $ (d0 +) <$> dist p s pnt)
	where
	Just x `sel` Just y = Just $ min x y
	Just x `sel` _ = Just x
	_ `sel` Just y = Just y
	_ `sel` _ = Nothing

main :: IO ()
main = do
	fp : _ <- getArgs
	src <- readFile fp
	let t = readTable' src
	print . lookup 100 $ unfoldr (next t) $ (1, Just 0) : map (, Nothing) [2 .. 100]
