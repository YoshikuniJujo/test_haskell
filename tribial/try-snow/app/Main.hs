{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Concurrent
import Data.List
import System.Random hiding (next)

main :: IO ()
main = snow 80 50 1 500000 []

snow :: Int -> Int -> Int -> Int -> [String] -> IO ()
snow w h n spd ls = do
	threadDelay spd
	ls' <- frame w h n ls
	snow w h n spd ls'
	

frame :: Int -> Int -> Int -> [String] -> IO [String]
frame w h n ls = do
	ss <- makeSnow 0 (w - 1) n
	let	l = showSnow ss
		ls' = next h l ls
	putStrLn `mapM_` ls'
	pure ls'

makeSnow :: Int -> Int -> Int -> IO [Int]
makeSnow mn mx n = do
	rs <- replicateM n $ randomRIO (mn, mx)
	pure . nub $ sort rs

showSnow :: [Int] -> String
showSnow is = ss $ zipWith (-) is (- 1 : is)
	where
	ss [] = ""
	ss (n : ns) = replicate (n - 1) ' ' ++ "*" ++ ss ns

next :: Int -> String -> [String] -> [String]
next h l ls = take h $ l : ls
