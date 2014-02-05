module Game (
	Disk(..), Game, Turn(..), X(..), Y(..),
	initGame, nextGame, turn, disks, placeable,
 ) where

import Control.Applicative ((<$>))
import Board (Disk(..), rev, Board, X(..), Y(..), initBoard)
import qualified Board (placeable, disks, place)

data Game = Game { turn :: Turn, board :: Board } deriving Show

disks :: Game -> [((X, Y), Disk)]
disks = Board.disks . board

placeable :: Game -> [(X, Y)]
placeable (Game (Turn d) b) = Board.placeable b d
placeable _ = []

place :: Game -> (X, Y) -> Maybe Game
place (Game t@(Turn d) b) pos = Game t <$> Board.place b d pos
place _ _ = Nothing

pass :: Game -> Bool
pass = null . placeable

flipTurn, gameOver :: Game -> Game
flipTurn (Game (Turn d) b) = Game (Turn $ rev d) b
flipTurn g = g
gameOver (Game _ b) = Game GameOver b

data Turn = Turn Disk | GameOver deriving Show

initGame :: Game
initGame = Game (Turn Black) initBoard

nextGame :: Game -> (X, Y) -> Maybe Game
nextGame g pos = do
	g' <- flipTurn <$> place g pos
	if not $ pass g' then return g' else do
		let g'' = flipTurn g'
		if not $ pass g'' then return g'' else
			return $ gameOver g''
{-
	let g'' = flipTurn g'
	return $ case (pass g', pass g'') of
		(False, _) -> g'
		(_, False) -> g''
		_ -> gameOver g''
-}
