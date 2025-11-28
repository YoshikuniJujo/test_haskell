{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-x-partial #-}

module Data.Lifegame.Glider (
	G(..), Shape(..), LeftRight(..), UpDown(..), add, addGs,

	readGliders,

	shape0, shape1, shape2, shape3, printShape,

	Independent(..), independentToPattern,

	matchIndependent, allIndependent,
	searchIndependent, searchIndependentBottom,
	searchIndependentLives, searchIndependentLives', searchIndependentLives'',

	searchIndependentLivesBottom, checkBottomEdgeG,

	removeBottomGliders, removeBottomGliders',

	boards'

	) where

import Prelude hiding (Either(..))
import Control.Arrow
import Data.List qualified as L
import Lifegame.Words

import Text.Read

data G = G { shape :: Shape, leftRight :: LeftRight, upDown :: UpDown }
	deriving (Show, Eq, Ord)

data Shape = Shape0 | Shape1 | Shape2 | Shape3 deriving (Show, Read, Enum, Eq, Ord)
data LeftRight = Left | Right deriving (Show, Read, Enum, Eq, Ord)
data UpDown = Up | Down deriving (Show, Read, Enum, Eq, Ord)

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
	deriving (Show, Eq, Ord)

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

matchIndependentLives :: Independent -> Board -> [((Int, Int), G)]
matchIndependentLives ind brd =
	uncurry (independentPosToGlider ind) <$>
		matchBoardLives (independentToPattern ind) brd

matchIndependentLives' :: Independent -> Board -> ([(Int, Int)], [((Int, Int), G)])
matchIndependentLives' ind brd =
	second ((uncurry $ independentPosToGlider ind) <$>) $
		matchBoardLives' (independentToPattern ind) brd

matchMultiIndependentLives :: [Independent] -> Board -> ([(Int, Int)], [((Int, Int), G)])
matchMultiIndependentLives inds brd = second ((uncurry independentToG) <$>) $
	multiMatchBoardLives
		((\ind -> (ind, independentToPattern ind)) <$> inds) brd

matchMultiIndependentLivesBottom :: Int -> [Independent] -> Board -> ([(Int, Int)], [((Int, Int), G)])
matchMultiIndependentLivesBottom n inds brd = second ((uncurry independentToG) <$>) $
	multiMatchBoardLivesBottom n
		((\ind -> (ind, independentToPattern ind)) <$> inds) brd

matchMultiIndependentLivesTop :: Int -> [Independent] -> Board -> ([(Int, Int)], [((Int, Int), G)])
matchMultiIndependentLivesTop n inds brd = second ((uncurry independentToG) <$>) $
	multiMatchBoardLivesTop n
		((\ind -> (ind, independentToPattern ind)) <$> inds) brd

independentToG :: Independent -> (Int, Int) -> ((Int, Int), G)
independentToG ind = uncurry $ independentPosToGlider ind

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

searchIndependentLives :: Board -> [((Int, Int), G)]
searchIndependentLives bd = concat $ (`matchIndependentLives` bd) <$> allIndependent

searchIndependentLives' :: Board -> ([(Int, Int)], [((Int, Int), G)])
searchIndependentLives' bd =
	first (L.nub . L.sort) . concat2 $ (`matchIndependentLives'` bd) <$> allIndependent

searchIndependentLives'' :: Board -> ([(Int, Int)], [((Int, Int), G)])
searchIndependentLives'' = matchMultiIndependentLives allIndependent

searchIndependentLivesBottom :: Int -> Board -> ([(Int, Int)], [((Int, Int), G)])
searchIndependentLivesBottom n = matchMultiIndependentLivesBottom n allIndependent

searchIndependentLivesTop :: Int -> Board -> ([(Int, Int)], [((Int, Int), G)])
searchIndependentLivesTop n = matchMultiIndependentLivesTop n allIndependent

concat2 :: [([a], [b])] -> ([a], [b])
concat2 [] = ([], [])
concat2 ((xs, ys) : xsyss) =
	let (xss, yss) = concat2 xsyss in (xs ++ xss, ys ++ yss)

searchIndependentBottom :: Int -> Board -> [((Int, Int), G)]
searchIndependentBottom n bd =
	concat $ (\ind -> matchIndependentBottom n ind bd) <$> allIndependent

checkBottomEdgeG :: [(Int, Int)] -> [((Int, Int), G)] -> Bool
checkBottomEdgeG [] gls = not $ checkRightDownToLeftDown $ L.sort gls
checkBottomEdgeG _ _ = False

checkRightDownToLeftDown :: [((Int, Int), G)] -> Bool
checkRightDownToLeftDown [] = False
checkRightDownToLeftDown ((_, g) : gs)
	| checkDirection Right Down g = checkLeftDown gs
	| otherwise = checkRightDownToLeftDown gs

checkLeftDown :: [((Int, Int), G)] -> Bool
checkLeftDown = any (checkDirection Left Down . snd)

checkTopEdgeG :: [(Int, Int)] -> [((Int, Int), G)] -> Bool
checkTopEdgeG [] gls = not $ checkRightUpToLeftUp $ L.sort gls
checkTopEdgeG _ _ = False

checkRightUpToLeftUp :: [((Int, Int), G)] -> Bool
checkRightUpToLeftUp [] = False
checkRightUpToLeftUp ((_, g) : gs)
	| checkDirection Right Up g = checkLeftUp gs
	| otherwise = checkRightUpToLeftUp gs

checkLeftUp :: [((Int, Int), G)] -> Bool
checkLeftUp = any (checkDirection Left Up . snd)

checkDirection :: LeftRight -> UpDown -> G -> Bool
checkDirection lr0 ud0 G { leftRight = lr, upDown = ud } =
	lr == lr0 && ud == ud0

removeGliders :: Board -> [((Int, Int), G)] -> Board
removeGliders bd gls = removeAreas bd (toArea <$> gls)
	where toArea ((xo, yo), _) = (xo, yo, 3, 3)

data Change a = NG | OK | Changed a deriving Show

removeBottomGliders' :: Board -> Maybe Board
removeBottomGliders' brd = case removeBottomGliders brd of
	NG -> Nothing; OK -> Just brd; Changed brd' -> Just brd'

removeBottomGliders :: Board -> Change Board
removeBottomGliders brd
	| checkBottomEdge brd = let
		(lvs, gls) = searchIndependentLivesBottom 7 brd in
		if checkBottomEdgeG lvs gls
		then Changed $ removeGliders brd (filter (isBottomGlider brd) gls)
		else NG
	| otherwise = OK

isBottomGlider :: Board -> ((Int, Int), G) -> Bool
isBottomGlider brd ((_, y), _) = y == boardHeight brd - 3

removeTopGliders' :: Board -> Maybe Board
removeTopGliders' brd = case removeTopGliders brd of
	NG -> Nothing; OK -> Just brd; Changed brd' -> Just brd'

removeTopGliders :: Board -> Change Board
removeTopGliders brd
	| checkTopEdge brd = let
		(lvs, gls) = searchIndependentLivesTop 7 brd in
		if checkTopEdgeG lvs gls
		then Changed $ removeGliders brd (filter (isTopGlider brd) gls)
		else NG
	| otherwise = OK

isTopGlider :: Board -> ((Int, Int), G) -> Bool
isTopGlider _ ((_, y), _) = y == 0

boards' :: Board -> [Board]
boards' brd = brd :
	case removeTopGliders' =<< removeBottomGliders' (boardNext brd) of
		Nothing -> []
		Just brd' -> boards' brd'
