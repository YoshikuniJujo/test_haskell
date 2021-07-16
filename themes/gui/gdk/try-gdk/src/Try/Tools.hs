{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Try.Tools where

import Control.Concurrent

import Graphics.Gdk.Events
import Graphics.Gdk.EventStructures
import Graphics.Gdk.Values
import Graphics.Gdk.Windows
import Graphics.Gdk.Windows.GdkWindowAttr

import Try.Tools.DoWhile

mainLoopNew :: (forall s . GdkEventSealed s -> IO Bool) -> IO ()
mainLoopNew f = doWhile_ do
	threadDelay 100000
	doWhile $ gdkWithEvent \case
		Just e -> do
			b <- f e
			pure if b then Nothing else Just False
		Nothing -> pure $ Just True

defaultGdkWindowAttr :: GdkWindowAttr
defaultGdkWindowAttr = minimalGdkWindowAttr (gdkEventMaskMultiBits [
		GdkExposureMask, GdkButtonPressMask, GdkKeyPressMask, GdkPointerMotionMask, GdkButtonMotionMask,
		GdkButtonReleaseMask -- , gdkAllEventsMask
		])
	400 400 gdkInputOutput GdkWindowToplevel
