{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.C.Types
import Control.Monad
import Data.List
import Data.Word
import Data.Int
import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.GdkScreen
import Graphics.Gdk.Visuals
import Graphics.Gdk.Windows

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
	mapM_ print . map packGroup . group =<< mapM peekVisual =<< gdkScreenListVisuals scr
	putStr "gdkScreenIsComposited: "
	print =<< gdkScreenIsComposited scr
	print =<< gdkWindowGetWindowType =<< gdkScreenGetRootWindow scr
	print =<< maybe (pure []) (mapM gdkWindowGetWindowType)
		=<< gdkScreenGetToplevelWindows scr
	maybe (putStrLn "No Window Stack") (mapM_ printWindowStack) =<< gdkScreenGetWindowStack scr

printWindowStack :: GdkWindowAutoUnref -> IO ()
printWindowStack wau = withGdkWindowAutoUnref wau \w ->
	print =<< gdkWindowGetOrigin w

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

type PixelDetails = (Word32, Int32, Int32)

peekVisual :: GdkVisual ->
	IO (GdkVisualType, Int32, PixelDetails, PixelDetails, PixelDetails)
peekVisual v = do
	t <- gdkVisualGetVisualType v
	d <- gdkVisualGetDepth v
	r <- gdkVisualGetRedPixelDetails v
	g <- gdkVisualGetGreenPixelDetails v
	b <- gdkVisualGetBluePixelDetails v
	pure (t, d, r, g, b)

packGroup :: [a] -> (Int, a)
packGroup [] = error "bad"
packGroup xa@(x : _) = (length xa, x)
