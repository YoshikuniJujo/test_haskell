{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib (runZundoko) where

import System.Random

data ZunDoko = Z | D deriving (Show, Bounded)

instance Random ZunDoko where
	randomR (a, b) g =
		let (x, g') = (randomR (zunDokoToInt a, zunDokoToInt b) g) in
			(intToZunDoko x, g')
		where
		zunDokoToInt :: ZunDoko -> Int
		zunDokoToInt Z = 0
		zunDokoToInt D = 1
		intToZunDoko :: Int -> ZunDoko
		intToZunDoko 0 = Z
		intToZunDoko _ = D
	random = randomR (minBound, maxBound)

runZundoko :: IO ()
runZundoko = putStrLn . zundoko =<< newStdGen

zundoko :: StdGen -> String
zundoko g = zundokoStr `concatMap` zundokoGen g ++ "キ・ヨ・シ!"

zundokoStr :: ZunDoko -> String
zundokoStr Z = "ズン"; zundokoStr D = "ドコ"

zundokoGen :: StdGen -> [ZunDoko]
zundokoGen g = zundokoFinish $ randoms g

zundokoFinish :: [ZunDoko] -> [ZunDoko]
zundokoFinish (Z : Z : Z : Z : D : _) = [Z, Z, Z, Z, D]
zundokoFinish (zd : zds) = zd : zundokoFinish zds
zundokoFinish _ = []
