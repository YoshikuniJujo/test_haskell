module Game (
	Game,
	Turn(..),
	Stone(..),
	initGame,
	nextGame,
	turn,
	stones,
 ) where

import Control.Monad

import Board

data Game = Game { turn :: Turn, board :: Board } deriving Show

data Turn = BlackTurn | WhiteTurn | GameOver deriving Show

turnToStone :: Turn -> Maybe Stone
turnToStone BlackTurn = Just Black
turnToStone WhiteTurn = Just White
turnToStone _ = Nothing

revT :: Turn -> Turn
revT BlackTurn = WhiteTurn
revT WhiteTurn = BlackTurn
revT _ = GameOver

initGame :: Game
initGame = Game BlackTurn initBoard

nextGame :: Game -> (X, Y) -> Maybe Game
nextGame (Game GameOver _) _ = Nothing
nextGame (Game t b) pos = do
	s <- turnToStone t
	when (null $ putable b s) $ fail ""
	b' <- put b s pos
	let t' = if not $ null $ putable b' (rev s) then revT t
		else if not $ null $ putable b' s then t else GameOver
	return $ Game t' b'
