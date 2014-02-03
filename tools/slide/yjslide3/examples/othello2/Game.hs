module Game (
	Game, putGame, showGame, stones,
	X(..), Y(..),
	Turn(..),
	Stone(..), rev,
	initGame,
	nextGame,
	turn,
	putable,
 ) where

import Control.Monad

import Board

data Game = Game { turn :: Turn, board :: Board } deriving Show

stones :: Game -> [((X, Y), Stone)]
stones = stonesB . board

putable :: Game -> [(X, Y)]
putable g = putableB (board g) (stone $ turn g)

data Turn = Turn { stone :: Stone } | GameOver deriving Show

putGame :: Game -> IO ()
putGame = putStr . showGame

showGame :: Game -> String
showGame (Game t b) = show t ++ "\n" ++ showBoard b

initGame :: Game
initGame = Game (Turn Black) initBoard

nextGame :: Game -> (X, Y) -> Maybe Game
nextGame (Game GameOver _) _ = Nothing
nextGame (Game t b) pos = do
	when (null $ putableB b s) $ fail ""
	b' <- put b s pos
	let t' = if not $ null $ putableB b' (rev s) then Turn $ rev s
		else if not $ null $ putableB b' s then t else GameOver
	return $ Game t' b'
	where
	s = stone t
