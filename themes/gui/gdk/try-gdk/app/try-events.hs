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
	_ <- gdkDisplayOpen ""
	win <- gdkToplevelNew Nothing $ minimalGdkWindowAttr
		(gdkEventMaskMultiBits [GdkKeyPressMask]) 700 500
	gdkWindowShow win
	mainLoop 500000 \case
		GdkEventGdkDelete (gdkEventAny -> e) -> False <$ print e
		GdkEventGdkWindowState (gdkEventWindowState -> e) -> True <$ do
			print e
			print . gdkWindowStateList
				$ gdkEventWindowStateChangedMask e
			print . gdkWindowStateList
				$ gdkEventWindowStateNewWindowState e
		GdkEventGdkConfigure (gdkEventConfigure -> e) -> True <$ print e
		GdkEventGdkMap (gdkEventAny -> e) -> True <$ print e
		GdkEventGdkVisibilityNotify (gdkEventVisibility -> e) ->
			True <$ print e
		GdkEventGdkKeyPress (gdkEventKeyKeyval . gdkEventKey -> GdkKey_q) ->
			pure False
		GdkEventGdkKeyPress (gdkEventKey -> e) -> True <$ print e
		GdkEventGdkAny (gdkEventAny -> e) -> True <$ print e

mainLoop :: Int -> (forall s . GdkEvent s -> IO Bool) -> IO ()
mainLoop slp f = gdkWithEventGet \case
	Nothing -> threadDelay slp >> mainLoop slp f
	Just e -> f e >>= \case False -> pure (); True -> mainLoop slp f

gdkWithAllEvents :: (forall s . GdkEvent s -> IO a) -> IO [a]
gdkWithAllEvents f = gdkWithEventGet \case
	Nothing -> pure []
	Just e -> (:) <$> f e <*> gdkWithAllEvents f

gdkWithAllEvents_ :: (forall s . GdkEvent s -> IO a) -> IO ()
gdkWithAllEvents_ f = gdkWithEventGet \case
	Nothing -> pure ()
	Just e -> f e >> gdkWithAllEvents_ f
