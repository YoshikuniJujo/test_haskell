{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Graphics.Vty

main :: IO ()
main = do
	vty <- mkVty =<< standardIOConfig
	let	line0 = string (defAttr `withForeColor` green) "first line"
		line1 = string (defAttr `withBackColor` blue) "second line"
	update vty . picForImage $ line0 <-> line1
	e <- nextEvent vty
	shutdown vty
	print $ "Last event was: " ++ show e
