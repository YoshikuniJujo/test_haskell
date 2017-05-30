import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

main :: IO ()
main = do
	startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = do
	return window # set title "test canvas"
	return ()
