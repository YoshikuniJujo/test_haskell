{-# LANGUAGE TupleSections #-}

module Main where

import System.Environment
import Data.Maybe
import Control.Applicative
import Control.Monad

import TDList

main = do
	fp : _ <- getArgs
	dists <- readDists <$> readFile fp
	putStrLn $ showTDList dists
	putStr $ showTDList $ getRoute dists

next :: (Eq a, Num b, Ord b) => a -> TDList a b -> TDList a b
next v tdl = foldl (uncurry . setDist) [] [(f, t) | f <- keys, t <- keys]
	where
	keys = allKeys tdl
	setDist l f t = maybe l (set l f t) $
		minM (get tdl f t) $ liftM2 (+) (get tdl f v) (get tdl v t)

getRoute :: (Eq a, Num b, Ord b) => TDList a b -> TDList a b
getRoute tdl = foldr next tdl $ allKeys tdl

readDists :: String -> TDList Int Int
readDists = foldl (uncurry3 . set) [] . map (\[x, y, z] -> (x, y, z)) .
	map (map read . words) . lines

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

minM :: Ord a => Maybe a -> Maybe a -> Maybe a
minM (Just x) (Just y) = Just $ min x y
minM x Nothing = x
minM Nothing y = y
