module Board (
	Board, Stone(..), rev, X(..), Y(..),
	initBoard, put, putable, stones,
) where

import Prelude hiding (reverse)
import Control.Arrow (second)
import Data.Maybe (isJust)

import Tools

data Board = Board [[State]]

instance Show Board where
	show (Board b) = unlines $ map (concatMap ss) b
		where
		ss (Stone Black) = "*|"
		ss (Stone White) = "O|"
		ss Empty = "_|"

data State = Stone { stone :: Stone } | Empty deriving (Eq, Show)

isStone :: State -> Bool
isStone (Stone _) = True
isStone _ = False

modifyStone :: (Stone -> Stone) -> State -> State
modifyStone f (Stone s) = Stone $ f s
modifyStone _ _ = Empty

data Stone = Black | White deriving (Eq, Show)

rev :: Stone -> Stone
rev Black = White
rev White = Black

data X = A | B | C | D | E | F | G | H
	deriving (Eq, Ord, Enum, Bounded, Show)
data Y = Y1 | Y2 | Y3 | Y4 | Y5 | Y6 | Y7 | Y8
	deriving (Eq, Ord, Enum, Bounded, Show)

initBoard :: Board
initBoard = Board $ map (map c2s) [
	"________",
	"________",
	"________",
	"___O*___",
	"___*O___",
	"________",
	"________",
	"________" ]
	where
	c2s '_' = Empty
	c2s '*' = Stone Black
	c2s 'O' = Stone White
	c2s _ = error "bad stone char"

put :: Board -> Stone -> (X, Y) -> Maybe Board
put b s pos = do
	b' <- set b s pos
	reverseLines b' s pos

putable :: Board -> Stone -> [(X, Y)]
putable b s = filter (isJust . reverseLines b s)
	[(x, y) | x <- [A .. H], y <- [Y1 .. Y8], not $ isStone $ get b (x, y)]

stones :: Board -> [((X, Y), Stone)]
stones b = map (second stone) $ filter (isStone . snd) $
	map (\pos -> (pos, get b pos)) [(x, y) | x <- [A .. H], y <- [Y1 .. Y8]]

----------------------------------------------------------------------
-- reverseLines :: Board -> Stone -> (X, Y) -> Maybe Board

reverseLines :: Board -> Stone -> (X, Y) -> Maybe Board
reverseLines brd stn (x, y) = foldMaybe op brd allDirections
	where
	op b (dx, dy) = do
		x' <- dx x
		y' <- dy y
		reverseLine b stn (x', y') (dx, dy)

type Direction = (X -> Maybe X, Y -> Maybe Y)

allDirections :: [Direction]
allDirections = [
	( prd,  prd), (Just,  prd), ( scc,  prd),
	( prd, Just),               ( scc, Just),
	( prd,  scc), (Just,  scc), ( scc,  scc)
 ]

reverseLine :: Board -> Stone -> (X, Y) -> Direction -> Maybe Board
reverseLine = reverseLineBool False

reverseLineBool :: Bool -> Board -> Stone -> (X, Y) -> Direction -> Maybe Board
reverseLineBool r b s0 (x, y) (dx, dy) = case get b (x, y) of
	Stone s -> if s == s0 then if r then Just b else Nothing else do
		x' <- dx x
		y' <- dy y
		reverseLineBool True (reverse b (x, y)) s0 (x', y') (dx, dy)
	_ -> Nothing

----------------------------------------------------------------------
-- get :: Board -> (X, Y) -> Stone
-- set :: Board -> Stone -> (X, Y) -> Maybe Board
-- reverse :: Board -> (X, Y) -> Board

get :: Board -> (X, Y) -> State
get (Board b) (x, y) = b !! fromEnum y !! fromEnum x

set :: Board -> Stone -> (X, Y) -> Maybe Board
set b s pos = case get b pos of
	Empty -> Just $ modify b (const $ Stone s) pos
	_ -> Nothing

reverse :: Board -> (X, Y) -> Board
reverse b = modify b $ modifyStone rev

modify :: Board -> (State -> State) -> (X, Y) -> Board
modify (Board b) s (x, y) =
	Board $ modifyList b (fromEnum y) (\l -> modifyList l (fromEnum x) s)
