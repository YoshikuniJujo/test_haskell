module Board (
	Disk(..), rev,
	Board, X(..), Y(..), initBoard, disks, placeable, place,
) where

import Data.Maybe (isJust)

import Tools (scc, prd, foldMaybe, modifyList)

data Board = Board [[Square]]

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

allSquare :: [(X, Y)]
allSquare = [ (x, y) | x <- [A .. H], y <- [Y1 .. Y8] ]

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
	b' <- set b s pos
	captureLines b' s pos

placeable :: Board -> Disk -> [(X, Y)]
placeable b s = [ p | p <- allSquare, isJust $ place b s p ]

disks :: Board -> [((X, Y), Disk)]
disks b = [ (p, disk s) | p <- allSquare, let s = get b p, isDisk s ]

----------------------------------------------------------------------
-- captureLines :: Board -> Disk -> (X, Y) -> Maybe Board

captureLines :: Board -> Disk -> (X, Y) -> Maybe Board
captureLines brd stn (x, y) = foldMaybe op brd allDirections
	where
	op b (dx, dy) = do
		x' <- dx x
		y' <- dy y
		captureLine b stn (x', y') (dx, dy)

type Direction = (X -> Maybe X, Y -> Maybe Y)

allDirections :: [Direction]
allDirections = [
	( prd,  prd), (Just,  prd), ( scc,  prd),
	( prd, Just),               ( scc, Just),
	( prd,  scc), (Just,  scc), ( scc,  scc)
 ]

captureLine :: Board -> Disk -> (X, Y) -> Direction -> Maybe Board
captureLine = captureLineBool False

captureLineBool :: Bool -> Board -> Disk -> (X, Y) -> Direction -> Maybe Board
captureLineBool r b s0 (x, y) (dx, dy) = case get b (x, y) of
	Disk s -> if s == s0 then if r then Just b else Nothing else do
		x' <- dx x
		y' <- dy y
		captureLineBool True (capture b (x, y)) s0 (x', y') (dx, dy)
	_ -> Nothing

----------------------------------------------------------------------
-- get :: Board -> (X, Y) -> Disk
-- set :: Board -> Disk -> (X, Y) -> Maybe Board
-- capture :: Board -> (X, Y) -> Board

get :: Board -> (X, Y) -> Square
get (Board b) (x, y) = b !! fromEnum y !! fromEnum x

set :: Board -> Disk -> (X, Y) -> Maybe Board
set b s pos = case get b pos of
	Empty -> Just $ modify b (const $ Disk s) pos
	_ -> Nothing

capture :: Board -> (X, Y) -> Board
capture b = modify b $ modifyDisk rev

modify :: Board -> (Square -> Square) -> (X, Y) -> Board
modify (Board b) s (x, y) =
	Board $ modifyList b (fromEnum y) (\l -> modifyList l (fromEnum x) s)
