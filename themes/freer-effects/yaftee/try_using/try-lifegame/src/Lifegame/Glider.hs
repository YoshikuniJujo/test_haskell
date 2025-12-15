{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-x-partial #-}

module Lifegame.Glider (

	-- * DATA TYPE

	G,

	-- * ADD GLIDERS

	addGs,

	-- * READ GLIDERS

	readGs,

	-- * BOARD OF EACH GENERATION

	generations'

	) where

import Prelude hiding (Either(..))
import Control.Arrow
import Data.List qualified as L
import Text.Read
import Lifegame.Board

-- DATA TYPE

data G = G { shape :: Shape, leftRight :: LeftRight, upDown :: UpDown }
	deriving (Show, Eq, Ord)

data Shape =
	Shape0 | Shape1 | Shape2 | Shape3 deriving (Show, Read, Enum, Eq, Ord)
data LeftRight = Left | Right deriving (Show, Read, Enum, Eq, Ord)
data UpDown = Up | Down deriving (Show, Read, Enum, Eq, Ord)

shapeAsAscii :: Shape -> [String]
shapeAsAscii = \case
	Shape0 -> shape0; Shape1 -> shape1; Shape2 -> shape2; Shape3 -> shape3
	where
	shape0 = ["*..", ".**", "**."]; shape1 = [".*.", "..*", "***"]
	shape2 = ["*.*", ".**", ".*."]; shape3 = ["..*", "*.*", ".**"]

rotate :: LeftRight -> UpDown -> [String] -> [String]
rotate Left Up = reverse . (reverse <$>); rotate Left Down = flipXY . reverse
rotate Right Up = reverse . flipXY; rotate Right Down = id

flipXY :: [String] -> [String]
flipXY s = (\x -> (!! x) <$> s) <$> [0 .. w - 1] where w = length $ head s

isDirectionOf :: (LeftRight, UpDown) -> G -> Bool
(h0, v0) `isDirectionOf` G { leftRight = h, upDown = v } = h == h0 && v == v0

-- ADD GLIDERS

addGs :: B -> [(G, (Int, Int))] -> B
addGs = foldl $ uncurry . flip . uncurry . add1

add1 :: B -> Int -> Int -> G -> B
add1 bd x y g = addShapeAscii bd x y
	. rotate (leftRight g) (upDown g) . shapeAsAscii $ shape g

-- READ GLIDERS

readGs :: [[String]] -> Maybe [(G, (Int, Int))]
readGs (dropWhile null -> []) = Just []
readGs (dropWhile null -> src) = uncurry (<$>) . ((:) *** readGs) =<< read1 src

read1 :: [[String]] -> Maybe ((G, (Int, Int)), [[String]])
read1 (	["shape:", spn] :
	["x-offset:", xo_] : ["y-offset:", yo_] :
	["left-right:", lr_] : ["up-down:", ud_] : rst) = do
	sp <- readMaybe $ "Shape" ++ spn
	xo <- readMaybe xo_; yo <- readMaybe yo_
	lr <- readMaybe lr_; ud <- readMaybe ud_
	pure ((G sp lr ud, (xo, yo)), rst)
read1 _ = Nothing

-- BOARD OF EACH GENERATION

generations' :: B -> [B]
generations' b =
	(b :) . maybe [] generations' $ removeTopGs =<< removeBttGs (next b)

-- Remove Top Gliders

removeTopGs :: B -> Maybe B
removeTopGs bd
	| checkTopEdge bd = case matchTop 7 allIndependentGs bd of
		([], (uncurry fromInd <$>) -> gls)
			| not . checkRightUpToLeftUp $ L.sort gls ->
				Just $ removeGs bd (filter isTop gls)
			| otherwise -> Nothing
		_ -> Nothing
	| otherwise = Just bd
	where isTop ((_, y), _) = y == 0

checkRightUpToLeftUp :: [((Int, Int), G)] -> Bool
checkRightUpToLeftUp [] = False
checkRightUpToLeftUp ((_, g) : gs)
	| (Right, Up) `isDirectionOf` g =
		any (((Left, Up) `isDirectionOf`) . snd) gs
	| otherwise = checkRightUpToLeftUp gs

-- Remove Bottom Gliders

removeBttGs :: B -> Maybe B
removeBttGs bd
	| checkBottomEdge bd = case matchBtt 7 allIndependentGs bd of
		([], (uncurry fromInd <$>) -> gls)
			| not $ checkRightDownToLeftDown $ L.sort gls ->
				Just $ removeGs bd (filter isBtt gls)
			| otherwise -> Nothing
		_ -> Nothing
	| otherwise = Just bd
	where isBtt ((_, y), _) = y == height bd - 3

checkRightDownToLeftDown :: [((Int, Int), G)] -> Bool
checkRightDownToLeftDown [] = False
checkRightDownToLeftDown ((_, g) : gs)
	| (Right, Down) `isDirectionOf` g =
		any (((Left, Down) `isDirectionOf`) . snd) gs
	| otherwise = checkRightDownToLeftDown gs

-- Remove Gliders

removeGs :: B -> [((Int, Int), G)] -> B
removeGs bd = clear bd . ((\((xo, yo), _) -> Area xo yo 3 3) <$>)

-- Independent

data Independent = Independent {
	indShape :: Shape, indLeftRight :: LeftRight, indUpDown :: UpDown }
	deriving (Show, Eq, Ord)

fromInd :: Independent -> (Int, Int) -> ((Int, Int), G)
fromInd Independent { indShape = s, indLeftRight = h, indUpDown = v } (x, y) =
	((x + 4, y + 4), G s h v)

allIndependentGs :: [(Independent, Pattern)]
allIndependentGs = (id &&& ptt) <$>
	[ Independent s h v | s <- [Shape0 ..], h <- [Left ..], v <- [Up ..] ]
	where
	ptt Independent { indShape = s, indLeftRight = h, indUpDown = v } =
		asciiToPattern 11 11 4 4 . rotate h v $ shapeAsAscii s
