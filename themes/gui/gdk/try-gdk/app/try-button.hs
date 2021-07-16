{-# LANGUAGE BlockArguments, LambdaCase #-}
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
			GdkButtonPressMask, GdkButtonReleaseMask ] }
	gdkWindowShow w
	mainLoopNew \case
		GdkEventSealedGdkKeyPress k -> case gdkEventKey k of
			GdkEventKey { gdkEventKeyKeyval = GdkKey_q } -> pure False
			_ -> pure True
		GdkEventSealedGdkButtonPress b -> True <$
			putStrLn ("GdkButtonPress: " ++ show (gdkEventButton b))
		GdkEventSealedGdkDoubleButtonPress b -> True <$
			putStrLn ("GdkDoubleButtonPress: " ++ show (gdkEventButton b))
		GdkEventSealedGdkTripleButtonPress b -> True <$
			putStrLn ("GdkTripleButtonPress: " ++ show (gdkEventButton b))
		GdkEventSealedGdkButtonRelease b -> True <$
			putStrLn ("GdkButtonRelease: " ++ show (gdkEventButton b))
		e -> True <$ print e
