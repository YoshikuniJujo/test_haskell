module Othello (
	Game,
	Stone(..),
	initGame,
	nextGame,
	aiGame,
	ai,
	board,
) where

import Control.Applicative
import Control.Concurrent
import Control.Exception

game :: Game -> IO ()
game g = do
	g' <- yourTurn g
	putStrLn $ show g'
	threadDelay 1000000
	case nextGame g' $ ai g' of
		Just g'' -> game g''
		_ -> error "bad ai"

yourTurn :: Game -> IO Game
yourTurn g = do
	putStrLn $ show g
	mg' <- nextGame g <$> inputLoop -- (readIO =<< getLine)
	case mg' of
		Nothing -> yourTurn g
		Just g' -> return g'

inputLoop :: IO (Int, Int)
inputLoop = (readIO =<< getLine) `catch` (const inputLoop :: IOError -> IO (Int, Int))

aiGame :: Game -> (Int, Int) -> Maybe Game
aiGame g pos = do
	g' <- nextGame g pos
	nextGame g' $ ai g'

ai :: Game -> (Int, Int)
ai (Game s b)
	| not $ null notBad = head notBad
	| otherwise = head can
	where
	good = filter goodStone can
	notBad = filter notBadStone can
	can = filter (check b s) [(x, y) | x <- [0 .. 7], y <- [0 .. 7]]

goodStone :: (Int, Int) -> Bool
goodStone (x, y)
	| x == 0 || y == 0 || x == 7 || y == 7 = True
	| otherwise = False

notBadStone :: (Int, Int) -> Bool
notBadStone (x, y)
	| x == 0 || y == 0 || x == 7 || y == 7 = True
	| x == 1 || y == 1 || x == 6 || y == 6 = False
	| otherwise = True

data Game = Game Stone Board

board :: Game -> [[Stone]]
board (Game _ (Board b)) = b

instance Show Game where
	show (Game s b) = "Game " ++ show s ++ "\n" ++ show b

initGame :: Game
initGame = Game Black initBoard

nextGame :: Game -> (Int, Int) -> Maybe Game
nextGame (Game s b) pos = Game (rev s) <$> put b s pos

data Stone = Empty | Black | White deriving (Eq, Show)

rev :: Stone -> Stone
rev Black = White
rev White = Black
rev Empty = Empty

data Board = Board [[Stone]]

showBoard :: Board -> String
showBoard (Board b) = unlines $ map (concatMap ss) b
	where
	ss Empty = "_|"
	ss Black = "*|"
	ss White = "O|"

putBoard :: Board -> IO ()
putBoard = putStr . showBoard

readBoard :: String -> Board
readBoard = Board . rb
	where
	rb "" = []
	rb ('\n' : cs) = [] : rb cs
	rb (c : '|' : cs) = let t : ls = rb cs in (c2s c : t) : ls
	c2s '_' = Empty
	c2s '*' = Black
	c2s 'O' = White

instance Read Board where
	readsPrec _ s = [(readBoard s, "")]

instance Show Board where
	show = showBoard

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

testBoard :: Board
testBoard = read $ unlines [
	"_|_|_|_|_|_|_|_|",
	"_|O|_|_|_|_|_|_|",
	"_|_|O|_|_|_|_|_|",
	"_|_|_|O|*|_|_|_|",
	"_|_|_|*|*|_|_|_|",
	"_|_|_|_|_|_|_|_|",
	"_|_|_|_|_|_|_|_|",
	"_|_|_|_|_|_|_|_|"
 ]

modifyList :: [a] -> Int -> (a -> a) -> [a]
modifyList xs n f = take n xs ++ [f $ xs !! n] ++ drop (n + 1) xs

modify :: Board -> (Stone -> Stone) -> (Int, Int) -> Board
modify (Board b) s (x, y) = Board $ modifyList b y (\l -> modifyList l x s)

get :: Board -> (Int, Int) -> Stone
get (Board b) (x, y) = b !! y !! x

-- reversible :: Board -> (Int, Int) -> Stone -> [(Int, Int)]

put :: Board -> Stone -> (Int, Int) -> Maybe Board
put b s pos
	| check b s pos = Just $ reverseLines (modify b (const s) pos) s pos
	| otherwise = Nothing

allDirections = [
	(-1, -1), ( 0, -1), ( 1, -1),
	(-1,  0),           ( 1,  0),
	(-1,  1), ( 0,  1), ( 1,  1)
 ]

check :: Board -> Stone -> (Int, Int) -> Bool
check b s (x, y) = get b (x, y) == Empty &&
	any (check1 b s (x, y)) allDirections

reverseLines :: Board -> Stone -> (Int, Int) -> Board
reverseLines brd s (x, y) = foldl rl brd allDirections
	where
	rl :: Board -> (Int, Int) -> Board
	rl b (dx, dy) =  if check1 b s (x, y) (dx, dy)
		then reverseLine b s (x + dx, y + dy) (dx, dy) else b

reverseLine :: Board -> Stone -> (Int, Int) -> (Int, Int) -> Board
reverseLine b s (x, y) (dx, dy)
	| get b (x, y) == s = b
	| get b (x, y) == rev s =
		reverseLine (modify b rev (x, y)) s (x + dx, y + dy) (dx, dy)
	| otherwise = error "bad line"

check1 :: Board -> Stone -> (Int, Int) -> (Int, Int) -> Bool
check1 b s (x, y) (dx, dy)
	| x + dx < 0 || x + dx > 7 || y + dy < 0 || y + dy > 7 = False
	| get b (x + dx, y + dy) /= rev s = False
	| otherwise = check1' b s (x + 2 * dx, y + 2 * dy) (dx, dy)

check1' :: Board -> Stone -> (Int, Int) -> (Int, Int) -> Bool
check1' b s (x, y) (dx, dy)
	| x < 0 || x > 7 || y < 0 || y > 7 || get b (x, y) == Empty = False
	| get b (x, y) == s = True
	| otherwise = check1' b s (x + dx, y + dy) (dx, dy)
