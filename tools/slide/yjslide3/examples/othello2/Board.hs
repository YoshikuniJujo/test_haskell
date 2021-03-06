module Board (
	Board, initBoard, putBoard, showBoard,
	put, putableB, stonesB,
	Stone(..), rev,
	X(..), Y(..),
) where

import Prelude hiding (reverse)
import Control.Arrow (second)
import Data.Maybe (isJust)

data Board = Board [[State]]

data State = Stone { stone :: Stone } | Empty deriving (Eq, Show)

revS :: State -> State
revS (Stone s) = Stone $ rev s
revS _ = Empty

isStone :: State -> Bool
isStone (Stone _) = True
isStone _ = False

data Stone = Black | White deriving (Eq, Show)

rev :: Stone -> Stone
rev Black = White
rev White = Black

data X = A | B | C | D | E | F | G | H
	deriving (Eq, Ord, Enum, Bounded, Show)
data Y = Y1 | Y2 | Y3 | Y4 | Y5 | Y6 | Y7 | Y8
	deriving (Eq, Ord, Enum, Bounded, Show)

scc, prd :: (Ord a, Enum a, Bounded a) => a -> Maybe a
scc x	| x < maxBound = Just $ succ x
	| otherwise = Nothing
prd x	| x > minBound = Just $ pred x
	| otherwise = Nothing

instance Show Board where
	show (Board b) = unlines $ map (concatMap ss) b
		where
		ss Empty = "_|"
		ss (Stone Black) = "*|"
		ss (Stone White) = "O|"

showBoard :: Board -> String
showBoard b = unlines $ "  A B C D E F G H" :
	zipWith (\n s -> show n ++ " " ++ s) [1 :: Int .. 8] (lines $ show b)

putBoard :: Board -> IO ()
putBoard = putStr . showBoard

readBoard :: String -> Board
readBoard = Board . rb
	where
	rb "" = []
	rb ('\n' : cs) = [] : rb cs
	rb (c : '|' : cs) = let t : ls = rb cs in (c2s c : t) : ls
	rb _ = error "bad board string"
	c2s '_' = Empty
	c2s '*' = Stone Black
	c2s 'O' = Stone White
	c2s _ = error "bad stone char"

instance Read Board where
	readsPrec _ s = [(readBoard s, "")]

initBoard :: Board
initBoard = read $ unlines [
	"_|_|_|_|_|_|_|_|",
	"_|_|_|_|_|_|_|_|",
	"_|_|_|_|_|_|_|_|",
	"_|_|_|O|*|_|_|_|",
	"_|_|_|*|O|_|_|_|",
	"_|_|_|_|_|_|_|_|",
	"_|_|_|_|_|_|_|_|",
	"_|_|_|_|_|_|_|_|"
 ]

put :: Board -> Stone -> (X, Y) -> Maybe Board
put b s pos = do
	b' <- set b s pos
	reverseLines b' s pos

putableB :: Board -> Stone -> [(X, Y)]
putableB b s = filter (isJust . reverseLines b s) $
	filter (not . isStone . get b) $
	[(x, y) | x <- [A .. H], y <- [Y1 .. Y8]]

stonesB :: Board -> [((X, Y), Stone)]
stonesB b = map (second stone) $ filter (isStone . snd) $
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

foldMaybe :: (a -> b -> Maybe a) -> a -> [b] -> Maybe a
foldMaybe = foldMaybeBool False

foldMaybeBool :: Bool -> (a -> b -> Maybe a) -> a -> [b] -> Maybe a
foldMaybeBool True _ x [] = Just x
foldMaybeBool False _ _ [] = Nothing
foldMaybeBool j op x (y : ys) = case x `op` y of
	Just x' -> foldMaybeBool True op x' ys
	_ -> foldMaybeBool j op x ys

reverseLine :: Board -> Stone -> (X, Y) -> Direction -> Maybe Board
reverseLine = reverseLineBool False

reverseLineBool :: Bool -> Board -> Stone -> (X, Y) -> Direction -> Maybe Board
reverseLineBool r b s0 (x, y) (dx, dy)
	| s == Stone s0 = if r then Just b else Nothing
	| s == Stone (rev s0) = do
		x' <- dx x
		y' <- dy y
		reverseLineBool True (reverse b (x, y)) s0 (x', y') (dx, dy)
	| otherwise = Nothing
	where
	s = get b (x, y)

----------------------------------------------------------------------
-- get :: Board -> (X, Y) -> Stone
-- set :: Board -> Stone -> (X, Y) -> Maybe Board
-- reverse :: Board -> (X, Y) -> Board

get :: Board -> (X, Y) -> State
get (Board b) (x, y) = b !! fromEnum y !! fromEnum x

set :: Board -> Stone -> (X, Y) -> Maybe Board
set b s pos
	| get b pos == Empty = Just $ modify b (const $ Stone s) pos
	| otherwise = Nothing

reverse :: Board -> (X, Y) -> Board
reverse b = modify b revS

modify :: Board -> (State -> State) -> (X, Y) -> Board
modify (Board b) s (x, y) =
	Board $ modifyList b (fromEnum y) (\l -> modifyList l (fromEnum x) s)

modifyList :: [a] -> Int -> (a -> a) -> [a]
modifyList xs n f = take n xs ++ [f $ xs !! n] ++ drop (n + 1) xs
