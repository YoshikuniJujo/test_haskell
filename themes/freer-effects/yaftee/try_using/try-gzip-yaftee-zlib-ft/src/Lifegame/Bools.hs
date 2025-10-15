{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lifegame.Bools (
	Board,

	boardToGray,

	boards,

	listToBoard,

	putShapeAscii,
	readLifegame, glider, penta
	) where

import GHC.Generics

import Control.DeepSeq
import Data.Vector qualified as V
import Data.Ratio
import Data.Word
import Data.Bool
import Data.Image.Gray qualified as ImageI

data Board = Board {
	boardWidth :: Int, boardHeight :: Int, boardBody :: V.Vector Bool }
	deriving (Eq, Generic)

instance NFData Board

boardToGray :: Int -> Board -> ImageI.G
boardToGray n Board { boardWidth = w, boardHeight = h, boardBody = bd } =
	ImageI.G {
		ImageI.grayWidth = w * n,
		ImageI.grayHeight = h * n,
		ImageI.grayBody = V.generate (w * n * h * n) \i ->
			let	x = i `mod` (w * n); y = i `div` (w * n) in
				bool 0xff 0x00 (bd V.! (y `div` n * w + x `div` n)) }

boards :: Board -> [Board]
boards b = ((:) $!! b) $ boards (next b)

next :: Board -> Board
next Board { boardWidth = w, boardHeight = h, boardBody = bd } =
	Board {
		boardWidth = w, boardHeight = h,
		boardBody = V.generate (w * h) (calc w h bd) }

calc :: Int -> Int -> V.Vector Bool -> Int -> Bool
calc w h bd n
	| not l && length (filter id ls) == 3 = True
	| l && 2 <= length (filter id ls) && length (filter id ls) <= 3 = True
	| otherwise = False
	where
	xl = n `mod` w; yl = n `div` w
	ps = [ (x `mod` w, y `mod` h) |
		x <- [xl - 1 .. xl + 1], y <- [yl - 1 .. yl + 1],
		(x, y) /= (xl, yl) ]
	l = bd V.! (yl * w + xl)
	ls = (bd V.!) . (\(x, y) -> y * w + x) <$> ps

putBoard :: Board -> IO ()
putBoard = (putStrLn `mapM_`) . boardToAscii

boardToAscii :: Board -> [String]
boardToAscii = ((boolToAscii <$>) <$>) . boardToList

asciiToBoard :: [String] -> Board
asciiToBoard = listToBoard . ((asciiToBool <$>) <$>)

asciiToBool :: Char -> Bool
asciiToBool = \case '.' -> False; '*' -> True; _ -> error "bad"

boolToAscii :: Bool -> Char
boolToAscii = bool '.' '*'

putShape :: Int -> Int -> Int -> Int -> [[Bool]] -> [[Bool]]
putShape w h _ _ [] = replicate h $ replicate w False
putShape w h 0 0 (r : rs) =
	(r ++ replicate (w - length r) False) : putShape w (h - 1) 0 0 rs
putShape w h xo 0 rs = (False :) <$> putShape (w - 1) h (xo - 1) 0 rs
putShape w h xo yo rs = replicate w False : putShape w (h - 1) xo (yo - 1) rs

putShapeAscii :: Int -> Int -> Int -> Int -> [String] -> [[Bool]]
putShapeAscii w h xo yo = putShape w h xo yo . ((asciiToBool <$>) <$>)

listToBoard :: [[Bool]] -> Board
listToBoard bs = Board {
	boardWidth = w, boardHeight = h, boardBody = V.fromList $ concat bs }
	where w = length $ head bs; h = length bs

boardToList :: Board -> [[Bool]]
boardToList Board { boardWidth = w, boardBody = bd } = sep w $ V.toList bd

sep :: Int -> [a] -> [[a]]
sep _ [] = []
sep n xs = take n xs : sep n (drop n xs)

glider :: [String]
glider = [
	".*.",
	"..*",
	"***" ]

penta :: [String]
penta = ["**********"]

readLifegame :: [String] ->
	(Int, Int, Int, Int, Int, Int, Int, Ratio Word16, [String])
readLifegame src = case words <$> src of
	["ratio:", rt] :
		["width:", w] :
		["height:", h] :
		["x-offset:", xo] :
		["y-offset:", yo] :
		["first-frame:", drp] :
		["frame-number:", fn] :
		["delay:", dly] :
		["shape:"] : shp -> (
		read rt, read w, read h, read xo, read yo, read drp, read fn,
		read dly, head <$> shp )

	_ -> error "bad"
