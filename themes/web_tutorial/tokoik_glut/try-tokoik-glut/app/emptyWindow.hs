{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

-- https://tokoik.github.io/opengl/libglut.html#4

import Control.Monad
import System.Environment
import Graphics.UI.GLUT

main :: IO ()
main = do
	void . join $ initialize <$> getProgName <*> getArgs
	void $ createWindow =<< getProgName
	displayCallback $= return ()
	mainLoop
