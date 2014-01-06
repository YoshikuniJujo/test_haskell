import Game
import Tetris

main :: IO ()
main = runGame 400 (Right Tick) processKey initialState nextState display

display :: State -> GtkWidget -> IO ()
display s w = do
	drawBlocks (blocks s) w
	showPoint w $ point s
