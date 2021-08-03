{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.GdkScreen
import Graphics.Gdk.Visuals

main :: IO ()
main = do
	void $ gdkDisplayOpen ""
	mscr <- gdkScreenGetDefault
	print mscr
	maybe (putStrLn "No default screens") run mscr

run :: GdkScreen -> IO ()
run scr = do
	printVisual "System Visual" =<< gdkScreenGetSystemVisual scr
	maybe (putStrLn "No rgba visuals") (printVisual "Rgba Visual")
		=<< gdkScreenGetRgbaVisual scr

printVisual :: String -> GdkVisual -> IO ()
printVisual ttl v = do
	t <- gdkVisualGetVisualType v
	d <- gdkVisualGetDepth v
	r <- gdkVisualGetRedPixelDetails v
	g <- gdkVisualGetGreenPixelDetails v
	b <- gdkVisualGetBluePixelDetails v
	putStrLn ttl
	putStrLn $ "\tVisualType       : " ++ show t
	putStrLn $ "\tDepth            : " ++ show d
	putStrLn $ "\tRedPixelDetails  : " ++ show r
	putStrLn $ "\tGreenPixelDetails: " ++ show g
	putStrLn $ "\tBluePixelDetails : " ++ show b
