module Main where

-- import Lib

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import System.Exit

main :: IO ()
main = startGUI defaultConfig {
	jsPort = Just 8023,
	jsStatic = Just "../wwwroot"
	} setup

setup :: Window -> UI ()
setup window = do
	_ <- return window # set UI.title "Hello world!"
	button <- UI.button # set UI.text "Click me!"
	exitButton <- UI.button # set UI.text "Shut down!"
	_ <- getBody window #+ [element button, element exitButton]
	on UI.click button . const $
		element button # set UI.text "I have been clicked!"
	on UI.click exitButton . const $ liftIO exitSuccess
