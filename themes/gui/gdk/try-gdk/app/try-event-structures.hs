{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Concurrent

import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.Windows
import Graphics.Gdk.Windows.GdkWindowAttr
import Graphics.Gdk.Windows.GdkEventMask
import Graphics.Gdk.Events
import Graphics.Gdk.EventStructures
import Graphics.Gdk.EventStructures.GdkKeySyms

main :: IO ()
main = do
	_dpy <- gdkDisplayOpen ""
	win <- gdkToplevelNew Nothing
		$ minimalGdkWindowAttr (gdkEventMaskMultiBits [
			GdkKeyPressMask ]) 700 500
	gdkWindowShow win
	mainLoop 500000 \case
		GdkEventGdkDelete (gdkEventAny -> e) -> False <$
			(putStrLn "DELETE" >> print e)
		GdkEventGdkNothing (gdkEventAny -> e) -> True <$
			(putStrLn "NOTHING" >> print e)
--		GdkEventGdkKeyPress (gdkEventKey -> e) -> True <$ print e
		GdkEventGdkKeyPress
			(gdkEventKeyKeyval . gdkEventKey -> GdkKey_q) ->
			pure False
		GdkEventGdkAny (gdkEventAny -> e) -> True <$ print e

mainLoop :: Int -> (forall s . GdkEvent s -> IO Bool) -> IO ()
mainLoop slp f = gdkWithEvent \case
	Nothing -> threadDelay slp >> mainLoop slp f
	Just e -> f e >>= \case False -> pure (); True -> mainLoop slp f
