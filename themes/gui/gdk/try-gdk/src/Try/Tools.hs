{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Try.Tools (mainLoop, mainLoopDisplay, defaultGdkWindowAttr) where

import Control.Concurrent

import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.Events
import Graphics.Gdk.EventStructures
import Graphics.Gdk.Windows.GdkWindowAttr
import Graphics.Gdk.Windows.GdkEventMask

import Try.Tools.DoWhile

mainLoop :: (forall s . GdkEvent s -> IO Bool) -> IO ()
mainLoop f = doWhile_ do
	threadDelay 100000
	doWhile $ gdkWithEventGet \case
		Just e -> do
			b <- f e
			pure if b then Nothing else Just False
		Nothing -> pure $ Just True

mainLoopDisplay :: GdkDisplay -> (forall s . GdkEvent s -> IO Bool) -> IO ()
mainLoopDisplay d f = doWhile_ do
	threadDelay 100000
	doWhile $ gdkDisplayWithEvent d \case
		Just e -> do
			b <- f e
			pure if b then Nothing else Just False
		Nothing -> pure $ Just True

defaultGdkWindowAttr :: GdkWindowAttr
defaultGdkWindowAttr = minimalGdkWindowAttr (gdkEventMaskMultiBits [
		GdkExposureMask, GdkButtonPressMask, GdkKeyPressMask, GdkPointerMotionMask, GdkButtonMotionMask,
		GdkButtonReleaseMask -- , gdkAllEventsMask
		]) 400 400
