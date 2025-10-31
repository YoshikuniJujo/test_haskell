{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Lifegame.Glider (
	G(..), Shape(..), LeftRight(..), UpDown(..), add, addGs,

	readGliders,

	shape0, shape1

	) where

import Prelude hiding (Either(..))
import Lifegame.Words

import Text.Read

data G = G { shape :: Shape, leftRight :: LeftRight, upDown :: UpDown }
	deriving Show

data Shape = Shape0 | Shape1 | Shape2 | Shape3 deriving (Show, Read)
data LeftRight = Left | Right deriving (Show, Read)
data UpDown = Up | Down deriving (Show, Read)

-- Left, Right, Top, Bottom
-- Left, Right, Up, Down

addGs :: Board -> [(G, (Int, Int))] -> Board
addGs = foldl addGlider

addGlider :: Board -> (G, (Int, Int)) -> Board
addGlider bd (gd, (x, y)) = add bd x y gd

add :: Board -> Int -> Int -> G -> Board
add bd x y gld = addShapeAscii bd x y (rotate lr ud sp)
	where
	sp = shapeAsAscii $ shape gld
	lr = leftRight gld
	ud = upDown gld

shapeAsAscii :: Shape -> [String]
shapeAsAscii = \case
	Shape0 -> shape0; Shape1 -> shape1; Shape2 -> shape2; Shape3 -> shape3

rotate :: LeftRight -> UpDown -> [String] -> [String]
rotate Left Up sp = reverse $ reverse <$> sp
rotate Left Down sp = flipXY $ reverse sp
rotate Right Up sp = reverse $ flipXY sp
rotate Right Down sp = sp

flipXY :: [String] -> [String]
flipXY sp = (\x -> (!! x) <$> sp) <$> [0 .. w - 1]
	where
	w = length $ head sp

shape0, shape1, shape2, shape3 :: [String]
shape0 = ["*..", ".**", "**."]
shape1 = [".*.", "..*", "***"]
shape2 = ["*.*", ".**", ".*."]
shape3 = ["..*", "*.*", ".**"]

printShape :: [String] -> IO ()
printShape = (putStrLn `mapM_`)

readGliders :: [[String]] -> Maybe [(G, (Int, Int))]
readGliders [] = Just []
readGliders src = do
	let	src' = dropWhile null src
	(g1, src'') <- readGlider1 src'
	(g1 :) <$> readGliders src''

readGlider1 :: [[String]] -> Maybe ((G, (Int, Int)), [[String]])
readGlider1 (
	["shape:", spn] :
	["x-offset:", xo_] :
	["y-offset:", yo_] :
	["left-right:", lr_] :
	["up-down:", ud_] : rst) = do
	sp <- readMaybe $ "Shape" ++ spn
	xo <- readMaybe xo_
	yo <- readMaybe yo_
	lr <- readMaybe lr_
	ud <- readMaybe ud_
	pure ((G sp lr ud, (xo, yo)), rst)
readGlider1 _ = Nothing
