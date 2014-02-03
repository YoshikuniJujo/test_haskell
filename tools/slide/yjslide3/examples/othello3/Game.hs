module Game (
	Game, Turn(..), Stone(..), rev, X(..), Y(..),
	initGame, nextGame, turn, putable, stones,
 ) where

import Control.Applicative ((<$>))

import Board hiding (putable, stones)
import qualified Board

data Game = Game { turn :: Turn, board :: Board } deriving Show

nextTurn, gameOver :: Game -> Game
nextTurn (Game (Turn s) b) = Game (Turn $ rev s) b
nextTurn g = g
gameOver (Game _ b) = Game GameOver b

stones :: Game -> [((X, Y), Stone)]
stones = Board.stones . board

putable :: Game -> [(X, Y)]
putable g = Board.putable (board g) (stone $ turn g)

pass :: Game -> Bool
pass = null . putable

data Turn = Turn { stone :: Stone } | GameOver deriving Show

initGame :: Game
initGame = Game (Turn Black) initBoard

nextGame :: Game -> (X, Y) -> Maybe Game
nextGame (Game t@(Turn s) b) pos = do
	g' <- nextTurn . Game t <$> put b s pos
	let g'' = nextTurn g'
	return $ case (pass g', pass g'') of
		(False, _) -> g'
		(_, False) -> g''
		_ -> gameOver g''
nextGame _ _ = Nothing
