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

	boards'

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
rotate Left Up sp = reverse $ reverse <$> sp
rotate Left Down sp = flipXY $ reverse sp
rotate Right Up sp = reverse $ flipXY sp
rotate Right Down sp = sp

flipXY :: [String] -> [String]
flipXY sp = (\x -> (!! x) <$> sp) <$> [0 .. w - 1] where w = length $ head sp

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

boards' :: B -> [B]
boards' b = (b :) . maybe [] boards' $ removeTopGs =<< removeBttGs (next b)

-- Remove Top Gliders

removeTopGs :: B -> Maybe B
removeTopGs bd
	| checkTopEdge bd = let (lvs, gls) = searchIndependentLivesTop 7 bd in
		if checkTopEdgeG lvs gls
			then Just $ removeGliders bd (filter isTopGlider gls)
			else Nothing
	| otherwise = Just bd
	where isTopGlider ((_, y), _) = y == 0

searchIndependentLivesTop :: Int -> B -> ([(Int, Int)], [((Int, Int), G)])
searchIndependentLivesTop n = matchMultiIndependentLivesTop n allIndependent

matchMultiIndependentLivesTop :: Int -> [Independent] -> B -> ([(Int, Int)], [((Int, Int), G)])
matchMultiIndependentLivesTop n inds brd = second ((uncurry independentToG) <$>) $
	multiMatchTop n ((\ind -> (ind, independentToPattern ind)) <$> inds) brd

checkTopEdgeG :: [(Int, Int)] -> [((Int, Int), G)] -> Bool
checkTopEdgeG [] gls = not . checkRightUpToLeftUp $ L.sort gls
checkTopEdgeG _ _ = False

checkRightUpToLeftUp :: [((Int, Int), G)] -> Bool
checkRightUpToLeftUp [] = False
checkRightUpToLeftUp ((_, g) : gs)
	| (Right, Up) `isDirectionOf` g =
		any (((Left, Up) `isDirectionOf`) . snd) gs
	| otherwise = checkRightUpToLeftUp gs

-- Remove Bottom Gliders

removeBttGs :: B -> Maybe B
removeBttGs brd = case removeBottomGliders brd of
	NG -> Nothing; OK -> Just brd; Changed brd' -> Just brd'

removeBottomGliders :: B -> Change B
removeBottomGliders brd
	| checkBottomEdge brd = let
		(lvs, gls) = searchIndependentLivesBottom 7 brd in
		if checkBottomEdgeG lvs gls
		then Changed $ removeGliders brd (filter (isBottomGlider brd) gls)
		else NG
	| otherwise = OK

checkBottomEdgeG :: [(Int, Int)] -> [((Int, Int), G)] -> Bool
checkBottomEdgeG [] gls = not $ checkRightDownToLeftDown $ L.sort gls
checkBottomEdgeG _ _ = False

checkRightDownToLeftDown :: [((Int, Int), G)] -> Bool
checkRightDownToLeftDown [] = False
checkRightDownToLeftDown ((_, g) : gs)
	| (Right, Down) `isDirectionOf` g = checkLeftDown gs
	| otherwise = checkRightDownToLeftDown gs

checkLeftDown :: [((Int, Int), G)] -> Bool
checkLeftDown = any (((Left, Down) `isDirectionOf`) . snd)

isBottomGlider :: B -> ((Int, Int), G) -> Bool
isBottomGlider brd ((_, y), _) = y == height brd - 3

matchMultiIndependentLivesBottom :: Int -> [Independent] -> B -> ([(Int, Int)], [((Int, Int), G)])
matchMultiIndependentLivesBottom n inds brd = second ((uncurry independentToG) <$>) $
	multiMatchBtt n ((\ind -> (ind, independentToPattern ind)) <$> inds) brd

searchIndependentLivesBottom :: Int -> B -> ([(Int, Int)], [((Int, Int), G)])
searchIndependentLivesBottom n = matchMultiIndependentLivesBottom n allIndependent

-- Remove Gliders

removeGliders :: B -> [((Int, Int), G)] -> B
removeGliders bd gls = clear bd (toArea <$> gls)
	where toArea ((xo, yo), _) = (Area xo yo 3 3)

data Change a = NG | OK | Changed a deriving Show

-- Independent

data Independent = Independent {
	independentShape :: Shape,
	independentLeftRight :: LeftRight,
	independentUpDown :: UpDown }
	deriving (Show, Eq, Ord)

independentToPattern :: Independent -> Pattern
independentToPattern Independent {
	independentShape = sp,
	independentLeftRight = lr,
	independentUpDown = ud } =
	asciiToPattern 11 11 4 4 . rotate lr ud $ shapeAsAscii sp

independentToG :: Independent -> (Int, Int) -> ((Int, Int), G)
independentToG ind = uncurry $ independentPosToGlider ind

independentPosToGlider :: Independent -> Int -> Int -> ((Int, Int), G)
independentPosToGlider
	Independent {
		independentShape = sp,
		independentLeftRight = lr,
		independentUpDown = ud } x y = ((x + 4, y + 4), G sp lr ud)

allIndependent :: [Independent]
allIndependent = concat . concat $
	(<$> [Shape0 ..]) \sp -> (<$> [Left ..]) \lr -> (<$> [Up ..]) \ud ->
		Independent sp lr ud
