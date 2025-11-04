{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Lifegame.Glider (
	G(..), Shape(..), LeftRight(..), UpDown(..), add, addGs,

	readGliders,

	shape0, shape1, shape2, shape3, printShape,

	Independent(..), independentToPattern,

	matchIndependent, allIndependent,
	searchIndependent, searchIndependentBottom

	) where

import Prelude hiding (Either(..))
import Lifegame.Words

import Text.Read

data G = G { shape :: Shape, leftRight :: LeftRight, upDown :: UpDown }
	deriving Show

data Shape = Shape0 | Shape1 | Shape2 | Shape3 deriving (Show, Read, Enum)
data LeftRight = Left | Right deriving (Show, Read, Enum)
data UpDown = Up | Down deriving (Show, Read, Enum)

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

rotateShapeAsAscii :: Shape -> LeftRight -> UpDown -> [String]
rotateShapeAsAscii sp lr ud = rotate lr ud $ shapeAsAscii sp

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

data Independent = Independent {
	independentShape :: Shape,
	independentLeftRight :: LeftRight,
	independentUpDown :: UpDown }
	deriving Show

independentToPattern :: Independent -> Pattern
independentToPattern Independent {
	independentShape = sp,
	independentLeftRight = lr,
	independentUpDown = ud } =
	asciiToPattern 11 11 4 4 $ rotateShapeAsAscii sp lr ud

matchIndependent :: Independent -> Board -> [((Int, Int), G)]
matchIndependent ind brd =
	uncurry (independentPosToGlider ind) <$>
		matchBoard' (independentToPattern ind) brd

matchIndependentBottom :: Int -> Independent -> Board -> [((Int, Int), G)]
matchIndependentBottom n ind brd =
	uncurry (independentPosToGlider ind) <$>
		matchBoardBottom n (independentToPattern ind) brd

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

searchIndependent :: Board -> [((Int, Int), G)]
searchIndependent bd = concat $ (`matchIndependent` bd) <$> allIndependent

searchIndependentBottom :: Int -> Board -> [((Int, Int), G)]
searchIndependentBottom n bd =
	concat $ (\ind -> matchIndependentBottom n ind bd) <$> allIndependent
