import System.Directory
import Data.List

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

main :: IO ()
main = do
	startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = do
	_ <- return window # set title "editor"
	wrap <- UI.div #. "wrap"
		# set style [
			("width", "450px"), ("height", "300px"),
			("border", "solid black 1px"), ("overflow", "scroll") ]
		# set (attr "tabindex") "1"
	canvas <- UI.canvas #. "canvas"
		# set style [
			("width", "900px"), ("height", "900px"),
			("overflow", "scroll"),
			("border", "solid black 1px") ]
		# set (attr "tabindex") "1"
	UI.fillText "hello" (10, 10) canvas
	UI.fillText "あいうえお漢字" (10, 30) canvas
	UI.strokeText "hello" (10, 20) canvas
	UI.strokeText "bottom" (10, 400) canvas
	files <- UI.button # set UI.text "Open File"
	return wrap #+ [element canvas]
	_ <- getBody window #+ [element wrap, element files]
	_ <- liftIO . register (UI.click files) . const
		. runUI window
		$ showDirectory canvas
	return ()

showDirectory :: UI.Canvas -> UI ()
showDirectory c = do
	UI.clearCanvas c
	fps <- liftIO $ getDirectoryContents "/"
	UI.fillText (unwords $ sort fps) (10, 10) c
