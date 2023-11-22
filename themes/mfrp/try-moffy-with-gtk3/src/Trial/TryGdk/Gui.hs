{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TryGdk.Gui where

import Foreign.C.Types
import Control.Concurrent
import Control.Concurrent.STM
import Control.Moffy
import Control.Moffy.Event.Delete
import Control.Moffy.Event.Window
import Control.Moffy.Event.Mouse
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
	TVar (Map GdkWindow WindowId) -> Handle' IO (DeleteEvent :- WindowEv :+: MouseDown :- MouseMove :- KeyEv :+: 'Nil)
handleGdk nid i2w w2i =
	(H.expand $ handleWindowNew nid i2w w2i :: Handle' IO (WindowNew :- WindowDestroy :- 'Nil))
	`H.merge` handleWindowConfigureKey w2i

handleWindowNew ::
	TVar WindowId -> TVar (Map WindowId GdkWindow) ->
	TVar (Map GdkWindow WindowId) -> Handle' IO (Singleton WindowNew)
handleWindowNew nid i2w w2i (unSingleton -> WindowNewReq) = do
	w <- gdkToplevelNew Nothing $ minimalGdkWindowAttr
		(gdkEventMaskMultiBits [
			GdkKeyPressMask, GdkKeyReleaseMask,
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

handleWindowConfigureKey ::
	TVar (Map GdkWindow WindowId) -> Handle' IO (DeleteEvent :- WindowConfigure :- MouseDown :- MouseMove :- KeyEv :+: 'Nil)
handleWindowConfigureKey tw2i _ =
	gdkWithEvent \case
		Just (GdkEventGdkDelete e_) -> do
			e <- gdkEventAny e_
			w2i <- atomically $ readTVar tw2i
			let	i = w2i ! gdkEventAnyWindow e
			pure . Just . App.expand . App.Singleton $ OccDeleteEvent i
		Just (GdkEventGdkConfigure e_) -> do
			e <- gdkEventConfigure e_
			w2i <- atomically $ readTVar tw2i
			let	i = w2i ! gdkEventConfigureWindow e
				x = fromIntegral $ gdkEventConfigureX e
				y = fromIntegral $ gdkEventConfigureY e
				w = fromIntegral $ gdkEventConfigureWidth e
				h = fromIntegral $ gdkEventConfigureHeight e
			pure . Just . App.expand . App.Singleton . OccWindowConfigure i $ Configure (x, y) (w, h)
		Just (GdkEventGdkButtonPress e_) -> do
			e <- gdkEventButton e_
			w2i <- atomically $ readTVar tw2i
			let	w = w2i ! gdkEventButtonWindow e
				x = realToFrac $ gdkEventButtonX e
				y = realToFrac $ gdkEventButtonY e
			pure . Just $ App.expand
				(OccMouseMove w (x, y) App.>- App.Singleton (OccMouseDown w . numToButton $ gdkEventButtonButton e)
					:: EvOccs (MouseMove :- MouseDown :- 'Nil))
		Just (GdkEventGdkKeyPress e_) -> do
			e <- gdkEventKey e_
			w2i <- atomically $ readTVar tw2i
			let	w = w2i ! gdkEventKeyWindow e
			pure . Just . App.expand . App.Singleton . OccKeyDown w $ gdkEventKeyKeyval e
		Just (GdkEventGdkKeyRelease e_) -> do
			e <- gdkEventKey e_
			w2i <- atomically $ readTVar tw2i
			let	w = w2i ! gdkEventKeyWindow e
			pure . Just . App.expand . App.Singleton . OccKeyUp w $ gdkEventKeyKeyval e
		Just _ -> pure Nothing
		Nothing -> threadDelay 50000 >> pure Nothing

numToButton :: CUInt -> MouseBtn
numToButton 1 = ButtonLeft
numToButton 2 = ButtonMiddle
numToButton 3 = ButtonRight
numToButton n = ButtonUnknown $ fromIntegral n
