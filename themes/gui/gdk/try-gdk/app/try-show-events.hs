{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import System.Environment

import Graphics.Gdk.General
import Graphics.Gdk.Windows
import Graphics.Gdk.Windows.GdkWindowAttr
import Graphics.Gdk.Events
import Graphics.Gdk.EventStructures
import Graphics.Gdk.EventStructures.GdkKeySyms

import Try.Tools

main :: IO ()
main = do
	print =<< join (gdkInit <$> getProgName <*> getArgs)
	w <- gdkWindowNew Nothing defaultGdkWindowAttr {
		gdkWindowAttrEventMask = gdkEventMaskMultiBits [
			GdkKeyPressMask,
			GdkButtonPressMask, GdkButtonReleaseMask,
			GdkScrollMask, GdkTouchMask, GdkSmoothScrollMask ] }
	gdkWindowShow w
	print =<< gdkGetShowEvents
	mainLoopNew \case
		GdkEventSealedGdkKeyPress k -> case gdkEventKey k of
			GdkEventKey { gdkEventKeyKeyval = GdkKey_q } -> pure False
			_ -> pure True
		GdkEventSealedGdkButtonPress b -> True <$ do
			putStrLn ("GdkButtonPress: " ++ show (gdkEventButton b))
			print (1 :: Int)
		GdkEventSealedGdkDoubleButtonPress b -> True <$ do
			putStrLn ("GdkDoubleButtonPress: " ++ show (gdkEventButton b))
			print (2 :: Int)
		GdkEventSealedGdkTripleButtonPress b -> True <$ do
			putStrLn ("GdkTripleButtonPress: " ++ show (gdkEventButton b))
			print (3 :: Int)
		GdkEventSealedGdkButtonRelease b -> True <$ do
			putStrLn ("GdkButtonRelease: " ++ show (gdkEventButton b))
			print (1 :: Int)
		GdkEventSealedGdkScroll (gdkEventScroll -> s) -> True <$ print s
		GdkEventSealedGdkAny (gdkEventAny -> e) -> True <$ print e

getClickCount :: GdkEventSealed s -> Maybe Int
getClickCount = \case
	GdkEventSealedGdkButtonPress _ -> Just 1
	GdkEventSealedGdkButtonRelease _ -> Just 1
	GdkEventSealedGdkDoubleButtonPress _ -> Just 2
	GdkEventSealedGdkTripleButtonPress _ -> Just 3
	_ -> Nothing
