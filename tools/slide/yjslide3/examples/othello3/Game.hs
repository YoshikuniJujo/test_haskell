module Game (
	Game, Turn(..), Disk(..), rev, X(..), Y(..),
	initGame, nextGame, turn, putable, disks,
 ) where

import Control.Applicative ((<$>))

import Board hiding (putable, disks)
import qualified Board

data Game = Game { turn :: Turn, board :: Board } deriving Show

nextTurn, gameOver :: Game -> Game
nextTurn (Game (Turn s) b) = Game (Turn $ rev s) b
nextTurn g = g
gameOver (Game _ b) = Game GameOver b

disks :: Game -> [((X, Y), Disk)]
disks = Board.disks . board

putable :: Game -> [(X, Y)]
putable g = Board.putable (board g) (disk $ turn g)

pass :: Game -> Bool
pass = null . putable

data Turn = Turn { disk :: Disk } | GameOver deriving Show

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
