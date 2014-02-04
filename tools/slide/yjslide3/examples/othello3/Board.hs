module Board (
	Board, Disk(..), rev, X(..), Y(..),
	initBoard, put, putable, disks,
) where

import Prelude hiding (reverse)
import Control.Arrow (second)
import Data.Maybe (isJust)

import Tools

data Board = Board [[State]]

instance Show Board where
	show (Board b) = unlines $ map (concatMap ss) b
		where
		ss (Disk Black) = "*|"
		ss (Disk White) = "O|"
		ss Empty = "_|"

data State = Disk { disk :: Disk } | Empty deriving (Eq, Show)

isDisk :: State -> Bool
isDisk (Disk _) = True
isDisk _ = False

modifyDisk :: (Disk -> Disk) -> State -> State
modifyDisk f (Disk s) = Disk $ f s
modifyDisk _ _ = Empty

data Disk = Black | White deriving (Eq, Show)

rev :: Disk -> Disk
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
	c2s '*' = Disk Black
	c2s 'O' = Disk White
	c2s _ = error "bad disk char"

put :: Board -> Disk -> (X, Y) -> Maybe Board
put b s pos = do
	b' <- set b s pos
	reverseLines b' s pos

putable :: Board -> Disk -> [(X, Y)]
putable b s = filter (isJust . reverseLines b s)
	[(x, y) | x <- [A .. H], y <- [Y1 .. Y8], not $ isDisk $ get b (x, y)]

disks :: Board -> [((X, Y), Disk)]
disks b = map (second disk) $ filter (isDisk . snd) $
	map (\pos -> (pos, get b pos)) [(x, y) | x <- [A .. H], y <- [Y1 .. Y8]]

----------------------------------------------------------------------
-- reverseLines :: Board -> Disk -> (X, Y) -> Maybe Board

reverseLines :: Board -> Disk -> (X, Y) -> Maybe Board
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

reverseLine :: Board -> Disk -> (X, Y) -> Direction -> Maybe Board
reverseLine = reverseLineBool False

reverseLineBool :: Bool -> Board -> Disk -> (X, Y) -> Direction -> Maybe Board
reverseLineBool r b s0 (x, y) (dx, dy) = case get b (x, y) of
	Disk s -> if s == s0 then if r then Just b else Nothing else do
		x' <- dx x
		y' <- dy y
		reverseLineBool True (reverse b (x, y)) s0 (x', y') (dx, dy)
	_ -> Nothing

----------------------------------------------------------------------
-- get :: Board -> (X, Y) -> Disk
-- set :: Board -> Disk -> (X, Y) -> Maybe Board
-- reverse :: Board -> (X, Y) -> Board

get :: Board -> (X, Y) -> State
get (Board b) (x, y) = b !! fromEnum y !! fromEnum x

set :: Board -> Disk -> (X, Y) -> Maybe Board
set b s pos = case get b pos of
	Empty -> Just $ modify b (const $ Disk s) pos
	_ -> Nothing

reverse :: Board -> (X, Y) -> Board
reverse b = modify b $ modifyDisk rev

modify :: Board -> (State -> State) -> (X, Y) -> Board
modify (Board b) s (x, y) =
	Board $ modifyList b (fromEnum y) (\l -> modifyList l (fromEnum x) s)
