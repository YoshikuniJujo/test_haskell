module Main where

import TDList
import Control.Applicative
import System.Environment

setT :: Eq a => TDList a b -> (a, a, b) -> TDList a b
setT tdl (i, j, v) = set tdl i j v

setTM :: Eq a => TDList a b -> (a, a, Maybe b) -> TDList a b
setTM tdl (i, j, Just v) = set tdl i j v
setTM tdl _ = tdl

readDists :: [(Int, Int, Int)] -> TDList Int Int
readDists t = foldl setT [] t

getThree :: [a] -> (a, a, a)
getThree [x, y, z] = (x, y, z)

next :: (Eq a, Num b, Ord b) => a -> TDList a b -> TDList a b
next v tdl = foldl setTM [] $ [(i, j, d) | i <- allKeys tdl, j <- allKeys tdl,
	let d = getMin tdl i j v]

getMin :: (Eq a, Num b, Ord b) => TDList a b -> a -> a -> a -> Maybe b
getMin tdl f t v
	| Just ft_ <- ft, Just fvt_ <- fvt = min ft fvt
	| Nothing <- ft = fvt
	| Nothing <- fvt = ft
	where
	ft = get tdl f t
	fvt = do
		fv <- get tdl f v
		vt <- get tdl v t
		return $ fv + vt

getRoute :: (Eq a, Num b, Ord b) => TDList a b -> TDList a b
getRoute tdl = foldr next tdl (allKeys tdl)

main = do
	fp : _ <- getArgs
	dists <- readDists . map (getThree . (map read . words)) . lines <$>
		readFile fp
	putStrLn $ showTDList dists
--	putStrLn $ showTDList $ next 1 dists
	putStr $ showTDList $ getRoute dists
