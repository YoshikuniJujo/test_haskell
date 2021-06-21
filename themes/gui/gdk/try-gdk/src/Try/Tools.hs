{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Try.Tools where

import Control.Concurrent
import Data.Bool

import Graphics.Gdk.Events
import Graphics.Gdk.Types
import Graphics.Gdk.Values

mainLoop :: (GdkEvent -> IO Bool) -> IO ()
mainLoop f = doWhile_ do
	threadDelay 100000
	doWhile $ gdkEventGet >>= \case
		Just e -> do
			b <- f e
			pure if b then Nothing else Just False
		Nothing -> pure $ Just True

doWhile_ :: Monad m => m Bool -> m ()
doWhile_ act = bool (pure ()) (doWhile_ act) =<< act

doWhile :: Monad m => m (Maybe a) -> m a
doWhile act = maybe (doWhile act) pure =<< act

defaultGdkWindowAttr :: GdkWindowAttr
defaultGdkWindowAttr = mkGdkWindowAttr [
		gdkExposureMask, gdkButtonPressMask, gdkKeyPressMask, gdkPointerMotionMask, gdkButtonMotionMask,
		gdkButtonReleaseMask, gdkAllEventsMask
		]
	400 400 gdkInputOutput GdkWindowToplevel
