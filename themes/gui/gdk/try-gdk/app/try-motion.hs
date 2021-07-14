{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Data.Char
import System.Environment

import Graphics.Gdk.General
import Graphics.Gdk.Windows
import Graphics.Gdk.EventStructures
import Graphics.Gdk.EventStructures.GdkKeySyms
import Try.Tools

main :: IO ()
main = do
	print =<< join (gdkInit <$> getProgName <*> getArgs)
	w <- gdkWindowNew Nothing defaultGdkWindowAttr
	gdkWindowSetEventCompression w False
	gdkWindowSetTitle w "あいうえお"
	gdkWindowShow w
	mainLoop \case
		GdkEventGdkDelete _d -> pure False
		GdkEventGdkKeyPress GdkEventKey { gdkEventKeyKeyval = GdkKey_q } -> pure False
		GdkEventGdkMotionNotify m -> do
			print m
			m' <- tryGdkEventMotionCopy m
			print m'
			True <$ print (gdkEventMotionRawAxes m == gdkEventMotionRawAxes m')
		e -> True <$ print e
