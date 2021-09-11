{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TryGdk.Gui where

import Control.Concurrent.STM
import Control.Moffy.Event.Window
import Control.Moffy.Event.Key
import Control.Moffy.Handle as H
import Data.Type.Set
import Data.OneOrMore
import Data.Map
import Graphics.Gdk.Windows
import Graphics.Gdk.Windows.GdkWindowAttr
import Graphics.Gdk.Windows.GdkEventMask
import Graphics.Gdk.Events
import Graphics.Gdk.EventStructures

import qualified Data.OneOrMoreApp as App

handleGdk :: TVar WindowId -> TVar (Map WindowId GdkWindow) ->
	TVar (Map GdkWindow WindowId) -> Handle' IO (WindowNew :- WindowDestroy :- KeyDown :- 'Nil)
handleGdk nid i2w w2i =
	(H.expand $ handleWindowNew nid i2w w2i :: Handle' IO (WindowNew :- WindowDestroy :- 'Nil))
	`H.merge` handleKeyDown w2i

handleWindowNew ::
	TVar WindowId -> TVar (Map WindowId GdkWindow) ->
	TVar (Map GdkWindow WindowId) -> Handle' IO (Singleton WindowNew)
handleWindowNew nid i2w w2i (unSingleton -> WindowNewReq) = do
	w <- gdkToplevelNew Nothing $ minimalGdkWindowAttr
		(gdkEventMaskMultiBits [
			GdkButtonPressMask, GdkButtonReleaseMask,
			GdkButtonMotionMask ]) 700 500
	gdkWindowSetEventCompression w False
	gdkWindowShow w
	r <- atomically do
		i@(WindowId i') <- readTVar nid
		writeTVar nid $ WindowId (i' + 1)
		modifyTVar i2w $ insert i w
		modifyTVar w2i $ insert w i
		pure . Just . App.Singleton $ OccWindowNew i
	pure r

handleKeyDown :: TVar (Map GdkWindow WindowId) -> Handle' IO (Singleton KeyDown)
handleKeyDown tw2i _ = gdkWithEvent \case
	Just (GdkEventGdkKeyPress e_) -> do
		e <- gdkEventKey e_
		print e
		w2i <- atomically $ readTVar tw2i
		let	w = w2i ! gdkEventKeyWindow e
		pure . Just . App.Singleton . OccKeyDown w $ gdkEventKeyKeyval e
	_ -> pure Nothing
