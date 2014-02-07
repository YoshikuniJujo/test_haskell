module Board (
	Disk(..), rev,
	Board, X(..), Y(..), initBoard,
	disks, placeable, place
) where

import Data.Maybe (isJust)
import Tools (scc, prd, modifyList, foldlMaybe)

data Disk = Black | White deriving (Eq, Show)

rev :: Disk -> Disk
rev Black = White
rev White = Black

data Square = Disk { disk :: Disk } | Empty deriving (Eq, Show)

isDisk :: Square -> Bool
isDisk (Disk _) = True
isDisk _ = False

modifyDisk :: (Disk -> Disk) -> Square -> Square
modifyDisk f (Disk d) = Disk $ f d
modifyDisk _ s = s

newtype Board = Board [[Square]]

instance Show Board where
	show (Board b) = unlines $ map (concatMap sd) b
		where
		sd (Disk Black) = "*|"
		sd (Disk White) = "O|"
		sd Empty = "_|"

initBoard :: Board
initBoard = Board $ map (map c2d) [
	"________",
	"________",
	"________",
	"___O*___",
	"___*O___",
	"________",
	"________",
	"________" ]
	where
	c2d '_' = Empty
	c2d '*' = Disk Black
	c2d 'O' = Disk White

data X = A | B | C | D | E | F | G | H
	deriving (Eq, Ord, Enum, Bounded, Show)
data Y = Y1 | Y2 | Y3 | Y4 | Y5 | Y6 | Y7 | Y8
	deriving (Eq, Ord, Enum, Bounded, Show)

allSquares :: [(X, Y)]
allSquares = [ (x, y) | x <- [A .. H], y <- [Y1 .. Y8] ]

type Direction = (X -> Maybe X, Y -> Maybe Y)

move :: Direction -> (X, Y) -> Maybe (X, Y)
move (dx, dy) (x, y) = do
	x' <- dx x
	y' <- dy y
	return (x', y')

allDirs :: [Direction]
allDirs = [
	( prd,  prd), (Just,  prd), ( scc,  prd),
	( prd, Just),               ( scc, Just),
	( prd,  scc), (Just,  scc), ( scc,  scc) ]

----------------------------------------------------------------------
-- get :: Board -> (X, Y) -> Disk
-- put :: Board -> Disk -> (X, Y) -> Maybe Board
-- cap :: Board -> (X, Y) -> Board

get :: Board -> (X, Y) -> Square
get (Board b) (x, y) = b !! fromEnum y !! fromEnum x

modifySquare :: Board -> (Square -> Square) -> (X, Y) -> Board
modifySquare (Board b) f (x, y) = Board $
	modifyList b (fromEnum y) $ \l -> modifyList l (fromEnum x) f

put :: Board -> Disk -> (X, Y) -> Maybe Board
put b d p = case get b p of
	Empty -> Just $ modifySquare b (const $ Disk d) p
	_ -> Nothing

cap :: Board -> (X, Y) -> Board
cap b = modifySquare b $ modifyDisk rev

----------------------------------------------------------------------
-- put :: Board -> Disk -> (X, Y) -> Maybe Board
-- capture :: Board -> Disk -> (X, Y) -> Maybe Board

capture :: Board -> Disk -> (X, Y) -> Maybe Board
capture brd dsk pos = foldlMaybe (\b -> capture1 b dsk pos) brd allDirs

capture1 :: Board -> Disk -> (X, Y) -> Direction -> Maybe Board
capture1 b d p dir = do
	p' <- move dir p
	capture1Bool False b d p' dir

capture1Bool :: Bool -> Board -> Disk -> (X, Y) -> Direction ->  Maybe Board
capture1Bool c b d0 p dir = case get b p of
	Disk d -> case (d == d0, c) of
		(False, _) -> do
			p' <- move dir p
			capture1Bool True (cap b p) d0 p' dir
		(_, True) -> Just b
		_ -> Nothing
	_ -> Nothing

----------------------------------------------------------------------
-- disks :: Board -> [((X, Y), Disk)]
-- placeable :: Board -> Disk -> [(X, Y)]
-- place :: Board -> Disk -> (X, Y) -> Maybe Board

disks :: Board -> [((X, Y), Disk)]
disks b = [ (p, disk s) | p <- allSquares, let s = get b p, isDisk s ]

place :: Board -> Disk -> (X, Y) -> Maybe Board
place b s pos = do
	b' <- put b s pos
	capture b' s pos

placeable :: Board -> Disk -> [(X, Y)]
placeable b d = [ p | p <- allSquares, isJust $ place b d p ]
