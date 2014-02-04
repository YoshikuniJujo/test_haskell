module Game (
	Disk(..), Game, Turn(..), X(..), Y(..),
	initGame, nextGame, turn, disks, placeable,
 ) where

import Control.Applicative ((<$>))
import Board (Disk(..), rev, Board, X(..), Y(..), initBoard, place)
import qualified Board (placeable, disks)

data Game = Game { turn :: Turn, board :: Board } deriving Show

disks :: Game -> [((X, Y), Disk)]
disks = Board.disks . board

placeable :: Game -> [(X, Y)]
placeable g = Board.placeable (board g) (disk $ turn g)

pass :: Game -> Bool
pass = null . placeable

flipTurn, gameOver :: Game -> Game
flipTurn (Game (Turn d) b) = Game (Turn $ rev d) b
flipTurn g = g
gameOver (Game _ b) = Game GameOver b

data Turn = Turn { disk :: Disk } | GameOver deriving Show

initGame :: Game
initGame = Game (Turn Black) initBoard

nextGame :: Game -> (X, Y) -> Maybe Game
nextGame (Game t@(Turn d) b) pos = do
	g' <- flipTurn . Game t <$> place b d pos
	let g'' = flipTurn g'
	return $ case (pass g', pass g'') of
		(False, _) -> g'
		(_, False) -> g''
		_ -> gameOver g''
nextGame _ _ = Nothing
