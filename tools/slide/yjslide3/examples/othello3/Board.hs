module Board (
	Disk(..), rev,
	Board, X(..), Y(..), initBoard, disks, placeable, place,
) where

import Control.Applicative ((<$>), (<*>))
import Data.Maybe (isJust)
import Tools (scc, prd, foldlMaybe, modifyList)

newtype Board = Board [[Square]]

instance Show Board where
	show (Board b) = unlines $ map (concatMap sd) b
		where
		sd (Disk Black) = "*|"
		sd (Disk White) = "O|"
		sd Empty = "_|"

data Square = Disk { disk :: Disk } | Empty deriving (Eq, Show)

isDisk :: Square -> Bool
isDisk (Disk _) = True
isDisk _ = False

modifyDisk :: (Disk -> Disk) -> Square -> Square
modifyDisk f (Disk d) = Disk $ f d
modifyDisk _ _ = Empty

data Disk = Black | White deriving (Eq, Show)

rev :: Disk -> Disk
rev Black = White
rev White = Black

data X = A | B | C | D | E | F | G | H
	deriving (Eq, Ord, Enum, Bounded, Show)
data Y = Y1 | Y2 | Y3 | Y4 | Y5 | Y6 | Y7 | Y8
	deriving (Eq, Ord, Enum, Bounded, Show)

allSquares :: [(X, Y)]
allSquares = [ (x, y) | x <- [A .. H], y <- [Y1 .. Y8] ]

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
	c2d _ = error "bad disk char"

place :: Board -> Disk -> (X, Y) -> Maybe Board
place b s pos = do
	b' <- put b s pos
	capture b' s pos

placeable :: Board -> Disk -> [(X, Y)]
placeable b s = [ p | p <- allSquares, isJust $ place b s p ]

disks :: Board -> [((X, Y), Disk)]
disks b = [ (p, disk s) | p <- allSquares, let s = get b p, isDisk s ]

----------------------------------------------------------------------
-- capture :: Board -> Disk -> (X, Y) -> Maybe Board
-- put :: Board -> Disk -> (X, Y) -> Maybe Board

type Direction = (X -> Maybe X, Y -> Maybe Y)

move :: Direction -> (X, Y) -> Maybe (X, Y)
move (dx, dy) (x, y) = (,) <$> dx x <*> dy y
{-
move (dx, dy) (x, y) = do
	x' <- dx x
	y' <- dy y
	return (x', y')
	-}

allDirections :: [Direction]
allDirections = [
	( prd,  prd), (Just,  prd), ( scc,  prd),
	( prd, Just),               ( scc, Just),
	( prd,  scc), (Just,  scc), ( scc,  scc) ]

capture :: Board -> Disk -> (X, Y) -> Maybe Board
capture brd dsk p = foldlMaybe cap1 brd allDirections
	where
	cap1 b dir = do
		p' <- move dir p
		capture1 b dsk p' dir

capture1 :: Board -> Disk -> (X, Y) -> Direction -> Maybe Board
capture1 = capture1Bool False

capture1Bool :: Bool -> Board -> Disk -> (X, Y) -> Direction -> Maybe Board
capture1Bool c b d0 p dir = case get b p of
	Disk d -> case (d == d0, c) of
		(False, _) -> do
			p' <- move dir p
			capture1Bool True (cap b p) d0 p' dir
		(_, True) -> Just b
		_ -> Nothing
	_ -> Nothing

----------------------------------------------------------------------
-- get :: Board -> (X, Y) -> Disk
-- put :: Board -> Disk -> (X, Y) -> Maybe Board
-- cap :: Board -> (X, Y) -> Board

get :: Board -> (X, Y) -> Square
get (Board b) (x, y) = b !! fromEnum y !! fromEnum x

put :: Board -> Disk -> (X, Y) -> Maybe Board
put b s pos = case get b pos of
	Empty -> Just $ modifySquare b (const $ Disk s) pos
	_ -> Nothing

cap :: Board -> (X, Y) -> Board
cap b = modifySquare b (modifyDisk rev)

modifySquare :: Board -> (Square -> Square) -> (X, Y) -> Board
modifySquare (Board b) f (x, y) =
	Board $ modifyList b (fromEnum y) (\l -> modifyList l (fromEnum x) f)
