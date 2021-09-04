{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import System.Environment

import Graphics.Gdk.General
import Graphics.Gdk.Windows
import Graphics.Gdk.Windows.GdkWindowAttr
import Graphics.Gdk.Windows.GdkEventMask
import Graphics.Gdk.EventStructures
import Graphics.Gdk.EventStructures.GdkKeySyms

import Try.Tools

main :: IO ()
main = do
	print =<< join (gdkInit <$> getProgName <*> getArgs)
	w <- gdkToplevelNew Nothing defaultGdkWindowAttr {
		gdkWindowAttrEventMask = gdkEventMaskMultiBits [
			GdkKeyPressMask,
			GdkButtonPressMask, GdkButtonReleaseMask,
			GdkScrollMask, GdkTouchMask, GdkSmoothScrollMask ] }
	gdkWindowShow w
	mainLoop \case
		GdkEventGdkKeyPress k -> gdkEventKey k >>= \case
			GdkEventKey { gdkEventKeyKeyval = GdkKey_q } -> pure False
			_ -> pure True
		GdkEventGdkButtonPress b -> True <$ do
			putStrLn . ("GdkButtonPress: " ++) . show =<< gdkEventButton b
			print (1 :: Int)
		GdkEventGdkDoubleButtonPress b -> True <$ do
			putStrLn . ("GdkDoubleButtonPress: " ++) . show =<< gdkEventButton b
			print (2 :: Int)
		GdkEventGdkTripleButtonPress b -> True <$ do
			putStrLn . ("GdkTripleButtonPress: " ++) . show =<< gdkEventButton b
			print (3 :: Int)
		GdkEventGdkButtonRelease b -> True <$ do
			putStrLn . ("GdkButtonRelease: " ++) . show =<< gdkEventButton b
			print (1 :: Int)
		GdkEventGdkScroll (gdkEventScroll -> s) -> True <$ print s
		GdkEventGdkAny e -> True <$ (print =<< gdkEventAny e)

getClickCount :: GdkEvent s -> Maybe Int
getClickCount = \case
	GdkEventGdkButtonPress _ -> Just 1
	GdkEventGdkButtonRelease _ -> Just 1
	GdkEventGdkDoubleButtonPress _ -> Just 2
	GdkEventGdkTripleButtonPress _ -> Just 3
	_ -> Nothing
