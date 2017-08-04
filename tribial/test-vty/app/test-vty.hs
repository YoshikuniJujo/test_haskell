module Main where

import Lib
import Graphics.Vty

main :: IO ()
main = do
	cfg <- standardIOConfig
	vty <- mkVty cfg
	let	line0 = string (defAttr `withForeColor` green) "first line"
		img = line0
		pic = picForImage img
	update vty pic
	e <- nextEvent vty
	shutdown vty
	print $ "Last event was: " ++ show e
