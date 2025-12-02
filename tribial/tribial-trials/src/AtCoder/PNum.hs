{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module AtCoder.PNum (PNum, fromShow, toShow) where

import Data.List qualified as L

data PNum = PNum { unPNum :: [Int] }

instance Eq PNum where
	PNum p1 == PNum p2 = p1 `eq` p2
		where
		[] `eq` [] = True
		[] `eq` (0 : ys) = [] `eq` ys
		(0 : xs) `eq` [] = xs `eq` []
		[] `eq` _ = False
		_ `eq` [] = False
		(x : xs) `eq` (y : ys) = x == y && xs `eq` ys

instance Num PNum where
	x1 + x2 = fromI @Integer $ toI x1 + toI x2
	x1 * x2 = fromI @Integer $ toI x1 * toI x2
	negate = PNum . (negate <$>) . unPNum
	signum = fromI . \case PNum [] -> 0; PNum (x : _) -> signum x
	abs x = case signum x of - 1 -> negate x; _ -> x
	fromInteger = fromI

instance Show PNum where
	show x = "(PNum " ++ L.intercalate "." ((show <$>) . (`toShowRaw` [1 ..]) $ unPNum x) ++ ")"

instance Enum PNum where
	toEnum = fromI
	fromEnum = toI

fromI :: Integral i => i -> PNum
fromI = PNum . go 1
	where
	go n = \case
		0 -> []
		x -> fromIntegral (x `mod` n) : go (n + 1) (x `div` n)

{-# SPECIALIZE fromI :: Int -> PNum #-}
{-# SPECIALIZE fromI :: Integer -> PNum #-}

toI :: Integral i => PNum -> i
toI = go 1 . unPNum
	where
	go n = \case
		[] -> 0
		x : xs -> fromIntegral x + go (n + 1) xs * n

{-# SPECIALIZE toI :: PNum -> Int #-}
{-# SPECIALIZE toI :: PNum -> Integer #-}

toShow :: Int -> PNum -> [Int]
toShow n (PNum ps) = toShowRaw (take n $ ps ++ repeat 0) [1 ..]

fromShow :: [Int] -> PNum
fromShow = PNum . (`fromShowRaw` [1 ..])

toShowRaw :: [Int] -> [Int] -> [Int]
toShowRaw = go . reverse
	where
	go [] _ = []
	go (p : ps) ns = y : go ps (filter (/= y) ns) where y = ns !! p

fromShowRaw :: [Int] -> [Int] -> [Int]
fromShowRaw = (reverse .) . go
	where
	go [] _ = []
	go (x : xs) ya@(y : _) = (x - y) : go (remove x xs) (remove x ya)
	go _ [] = error "bad"

remove :: Int -> [Int] -> [Int]
remove n = \case
	[] -> []
	x : xs	| x < n -> x : remove n xs
		| x > n -> x - 1 : remove n xs
		| otherwise -> remove n xs
