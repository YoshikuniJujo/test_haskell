{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lifegame.Words (

	Board, emptyBoard,

	boards,

	boardToGray1, boardToGray1', gray1ToBoard,

	putShapeAscii, addShapeAscii, printAsAscii,

	Pattern, asciiToPattern, printPatternAsAscii,

	match, matchLife, matchBoard,

	Clipped, clip, clip', printClippedAsAscii

	) where

import Prelude hiding (read)

import GHC.Generics

import Control.DeepSeq
import Data.Traversable
import Data.Bits
import Data.Maybe
import Data.Vector qualified as V
import Data.Bool
import Data.Word
import Data.Image.Gray1 qualified as Gray1

data Board = Board {
	boardWidth :: Int, boardHeight :: Int, boardBody :: V.Vector Word8 }
	deriving (Generic, Eq, Show)

instance NFData Board

boards :: Board -> [Board]
boards = iterate next

emptyBoard :: Int -> Int -> Board
emptyBoard w h = generate w h \_ _ -> False

next :: Board -> Board
next bd = generate (boardWidth bd) (boardHeight bd) (calc bd)

calc :: Board -> Int -> Int -> Bool
calc p cx cy
	| not l && length (filter id ls) == 3 = True
	| l && 2 <= length (filter id ls) && length (filter id ls) <= 3 = True
	| otherwise = False
	where
	ps = [ (x, y) |
		x <- [cx - 1 .. cx + 1], y <- [cy - 1 .. cy + 1],
		(x, y) /= (cx, cy) ]
	l = read p cx cy
	ls = (\(x, y) -> read p x y) <$> ps

addShape :: Board -> Int -> Int -> [[Bool]] -> Board
addShape bd xo yo bss = generate w h \x y ->
	if	xo <= x && x < xo + (length $ head bss) &&
		yo <= y && y < yo + (length bss)
	then	bss !! (y - yo) !! (x - xo)
	else	read bd x y
	where
	w = boardWidth bd
	h = boardHeight bd

addShapeAscii :: Board -> Int -> Int -> [String] -> Board
addShapeAscii bd xo yo = addShape bd xo yo . (((== '*') <$>) <$>)

putLife :: Board -> Int -> Int -> Board
putLife bd lx ly = generate (boardWidth bd) (boardHeight bd) \x y ->
	read bd x y || (x, y) == (lx, ly)

read :: Board -> Int -> Int -> Bool
read Board { boardWidth = w, boardHeight = h, boardBody = bd } x y =
	testBit (bd V.! i) bi
	where
	i = w' * y' + (x' `div` 8)
	bi = 7 - x' `mod` 8
	w' = (w - 1) `div` 8 + 1
	x' = x `mod` w
	y' = y `mod` h

generate :: Int -> Int -> (Int -> Int -> Bool) -> Board
generate w h px = Board {
	boardWidth = w, boardHeight = h,
	boardBody = generateBody w h px }

generateBody :: Int -> Int -> (Int -> Int -> Bool) -> V.Vector Word8
generateBody w h px = V.generate (w' * h) \i ->
		boolsToWord $ (<$> [0 .. 7]) \dx ->
			px' (i `mod` w' * 8 + dx) (i `div` w')
	where
	px' x y	| x >= w = False
		| otherwise = px x y
	w' = (w - 1) `div` 8 + 1

boolsToWord :: [Bool] -> Word8
boolsToWord bls = go 0 bls'
	where
	go r [] = r
	go r (b : bs') = go (bool id (`setBit` 0) b $ r `shiftL` 1) bs'
	bls' = bls ++ replicate (8 - length bls) False

boardToGray1 :: Board -> Gray1.G
boardToGray1 Board { boardWidth = w, boardHeight = h, boardBody = bd } =
	Gray1.G { Gray1.width = w, Gray1.height = h, Gray1.body = bd }

boardToGray1' :: Int -> Board -> Gray1.G
boardToGray1' n b = Gray1.generate (boardWidth b * n) (boardHeight b * n) \x y ->
	read b (x `div` n) (y `div` n)

gray1ToBoard :: Gray1.G -> Board
gray1ToBoard Gray1.G { Gray1.width = w, Gray1.height = h, Gray1.body = bd } =
	Board { boardWidth = w, boardHeight = h, boardBody = bd }

putShape :: Int -> Int -> Int -> Int -> [[Bool]] -> Board
putShape w h xo yo bss = generate w h \x y ->
	if	xo <= x && x < xo + (length $ head bss) &&
		yo <= y && y < yo + (length bss)
	then	bss !! (y - yo) !! (x - xo)
	else	False

putShapeBody :: Int -> Int -> Int -> Int -> [[Bool]] -> V.Vector Word8
putShapeBody w h xo yo bss = generateBody w h \x y ->
	if	xo <= x && x < xo + (length $ head bss) &&
		yo <= y && y < yo + (length bss)
	then	bss !! (y - yo) !! (x - xo)
	else	False

putShapeAscii :: Int -> Int -> Int -> Int -> [String] -> Board
putShapeAscii w h xo yo = putShape w h xo yo . (((== '*') <$>) <$>)

printAsAscii :: Board -> IO ()
printAsAscii = (putStrLn `mapM_`) . boardToAscii

printClippedAsAscii :: Clipped -> IO ()
printClippedAsAscii = (putStrLn `mapM_`) . clippedToAscii

boardToAscii :: Board -> [String]
boardToAscii b = (take (boardWidth b) . concat . (wordToAscii <$>) <$>) $ rows b

clippedToAscii :: Clipped -> [String]
clippedToAscii Clipped { clippedWidth = w, clippedHeight = h, clippedBody = bd } =
	bodyToAscii w h bd

bodyToAscii :: Int -> Int -> V.Vector Word8 -> [String]
bodyToAscii w h b = (take w . concat . (wordToAscii <$>) <$>) $ rowsBody w h b

rows :: Board -> [[Word8]]
rows Board { boardWidth = w, boardHeight = h, boardBody = bd } =
	(<$> [0 .. h - 1]) \y -> (<$> [0 .. (w - 1) `div` 8]) \x ->
		bd V.! (y * ((w - 1) `div` 8 + 1) + x)

rowsClipped :: Clipped -> [[Word8]]
rowsClipped Clipped { clippedWidth = w, clippedHeight = h, clippedBody = bd } =
	rowsBody w h bd

rowsBody :: Int -> Int -> V.Vector Word8 -> [[Word8]]
rowsBody w h bd = (<$> [0 .. h - 1]) \y -> (<$> [0 .. (w - 1) `div` 8]) \x ->
	bd V.! (y * ((w - 1) `div` 8 + 1) + x)

wordToAscii :: Word8 -> String
wordToAscii w = bool '.' '*' . testBit w <$> [7, 6 .. 0]

data Pattern = Pattern {
	patternLives :: [(Int, Int)],
	patternWidth :: Int,
	patternHeight :: Int,
	patternBody :: V.Vector Word8 }
	deriving Show

asciiToPattern :: Int -> Int -> Int -> Int -> [String] -> Pattern
asciiToPattern w h xo yo = boolsToPattern w h xo yo . (((== '*') <$>) <$>)

boolsToPattern :: Int -> Int -> Int -> Int -> [[Bool]] -> Pattern
boolsToPattern w h xo yo bs = Pattern {
	patternLives = boolsToLives xo yo bs,
	patternWidth = w, patternHeight = h,
	patternBody = putShapeBody w h xo yo bs }

boolsToLives :: Int -> Int -> [[Bool]] -> [(Int, Int)]
boolsToLives xo yo bs = go xo yo bs
	where
	go _ _ [] = []
	go x y ([] : rs) = go xo (y + 1) rs
	go x y ((lf : lvs) : rs) = bool id ((x, y) :) lf $ go (x + 1) y (lvs : rs)

printPatternAsAscii :: Pattern -> IO ()
printPatternAsAscii = (putStrLn `mapM_`) . patternToAscii

patternToAscii :: Pattern -> [String]
patternToAscii
	Pattern { patternWidth = w, patternHeight = h, patternBody = bd } =
	bodyToAscii w h bd

data Clipped = Clipped {
	clippedWidth :: Int, clippedHeight :: Int, clippedBody :: V.Vector Word8 }
	deriving (Generic, Eq, Show)

clip :: Board -> Int -> Int -> Int -> Int -> Clipped
clip Board { boardWidth = bw, boardHeight = bh, boardBody = bbd } cxo cyo cw ch =
	Clipped {
		clippedWidth = cw, clippedHeight = ch,
		clippedBody = clipBody bw bh bbd cxo cyo cw ch }

clip' :: Board -> Int -> Int -> Int -> Int -> Clipped
clip' Board { boardWidth = bw, boardHeight = bh, boardBody = bbd } cxo cyo cw ch =
	Clipped {
		clippedWidth = cw, clippedHeight = ch,
		clippedBody = clipBody' bw bh bbd cxo cyo cw ch }

clipBody ::
	Int -> Int -> V.Vector Word8 ->
	Int -> Int -> Int -> Int -> V.Vector Word8
clipBody w h bd xo yo cw ch = V.generate
	(((cw - 1) `div` 8 + 1) * ch) (clipBodyFun w h bd xo yo cw ch)

clipBody' ::
	Int -> Int -> V.Vector Word8 ->
	Int -> Int -> Int -> Int -> V.Vector Word8
clipBody' w h bd xo yo cw ch = V.generate
	(((cw - 1) `div` 8 + 1) * ch) (clipBodyFun' w h bd xo yo cw ch)

clipBodyFun ::
	Int -> Int -> V.Vector Word8 -> Int -> Int -> Int -> Int -> Int -> Word8
clipBodyFun w h bd cxo cyo cw ch i = if (cxow + xw) < (ww - 1)
	then combine cxob
		(bd V.! (((cyo + y) * ww) + cxow + xw))
		(bd V.! (((cyo + y) * ww) + cxow + xw + 1))
	else combine cxob (bd V.! (((cyo + y) * ww) + cxow + xw)) 0
	where
	xw = i `mod` cww
	y = i `div` cww
	ww = (w - 1) `div` 8 + 1
	cww = (cw - 1) `div` 8 + 1
	cxow = cxo `div` 8
	cxob = cxo `mod` 8

clipBodyFun' ::
	Int -> Int -> V.Vector Word8 -> Int -> Int -> Int -> Int -> Int -> Word8
clipBodyFun' w h bd cxo cyo cw ch i
	| (cxow + xw) < (- 1) || (cxow + xw) >= ww = 0
	| (cyo + y) < 0 || (cyo + y) >= h = 0
	| (cxow + xw) < 0 = combine cxob 0 (bd V.! (((cyo + y) * ww) + cxow + xw + 1))
	| (cxow + xw) < (ww - 1) = combine cxob
		(bd V.! (((cyo + y) * ww) + cxow + xw))
		(bd V.! (((cyo + y) * ww) + cxow + xw + 1))
	| otherwise = combine cxob (bd V.! (((cyo + y) * ww) + cxow + xw)) 0
	where
	xw = i `mod` cww
	y = i `div` cww
	ww = (w - 1) `div` 8 + 1
	cww = (cw - 1) `div` 8 + 1
	cxow = cxo `div` 8
	cxob = cxo `mod` 8

combine :: Int -> Word8 -> Word8 -> Word8
combine n b1 b2 = b1 `shiftL` n .|. b2 `shiftR` (8 - n)

patternToArea :: Int -> Int -> Pattern -> Int -> Int -> (Int, Int, Int, Int)
patternToArea px py Pattern { patternWidth = pw, patternHeight = ph } x y =
	(x - px, y - py, pw, ph)

matchLife :: Pattern -> Int -> Int -> Board -> Maybe (Int, Int)
matchLife pttn@Pattern { patternLives = lvs } bx by brd = go lvs
	where
	go [] = Nothing
	go ((px, py) : lvs) =
		bool (go lvs) (Just (px, py)) $ match px py pttn bx by brd

match :: Int -> Int -> Pattern -> Int -> Int -> Board -> Bool
match px py pttn bx by brd = matchClipped pttn $ clip' brd xo yo cw ch
	where
	(xo, yo, cw, ch) = patternToArea px py pttn bx by

matchClipped :: Pattern -> Clipped -> Bool
matchClipped Pattern { patternBody = pbd } Clipped { clippedBody = cbd } =
	pbd == cbd

matchBoard :: Pattern -> Board -> [((Int, Int), (Int, Int))]
matchBoard pttn brd@Board { boardWidth = w, boardHeight = h } = catMaybes . concat
	$ (<$> [0 .. h - 1]) \y -> (<$> [0 .. w - 1]) \x ->
		((x, y) ,) <$> matchLife pttn x y brd
