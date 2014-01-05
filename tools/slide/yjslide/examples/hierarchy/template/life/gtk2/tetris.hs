import Game
import Tetris

main :: IO ()
main = runGame 400 (Right Tick) processKey initialState nextState
	(drawBlocks . blocks)
