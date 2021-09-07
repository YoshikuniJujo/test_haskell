{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TryGdk where

import Foreign.C.Types
import Control.Concurrent
import Control.Concurrent.STM
import Data.Type.Set
import Data.OneOrMore
import qualified Data.OneOrMoreApp as App
import Data.Map
import Data.Time
import Data.Time.Clock.TAI

import Control.Moffy
import Control.Moffy.Event.Time
import Control.Moffy.Event.Window
import Control.Moffy.Event.Mouse
import Control.Moffy.Handle as H
import Control.Moffy.Handle.Time

import Graphics.Gdk.Windows
import Graphics.Gdk.Windows.GdkWindowAttr
import Graphics.Gdk.Windows.GdkEventMask
import Graphics.Gdk.Events
import Graphics.Gdk.EventStructures

handleGdk' :: TimeState s =>
	TVar WindowId ->
	TVar (Map WindowId GdkWindow) -> TVar (Map GdkWindow WindowId) -> (DiffTime, ()) ->
	HandleSt' s IO (WindowNew :- MouseDown :- TimeEv)
handleGdk' wid i2w w2i = popInput . handleTimeEvPlus . pushInput . const . liftHandle' $ handleGdk wid i2w w2i

handleGdk ::
	TVar WindowId ->
	TVar (Map WindowId GdkWindow) -> TVar (Map GdkWindow WindowId) ->
	Handle' IO (WindowNew :- MouseDown :- 'Nil)
handleGdk wid i2w w2i rqs = do
	handleWindowNew wid i2w w2i `H.merge` handleMouseDown w2i $ rqs

handleWindowNew ::
	TVar WindowId -> TVar (Map WindowId GdkWindow) ->
	TVar (Map GdkWindow WindowId) -> Handle' IO (Singleton WindowNew)
handleWindowNew nid i2w w2i (unSingleton -> WindowNewReq) = do
	w <- gdkToplevelNew Nothing $ minimalGdkWindowAttr (gdkEventMaskMultiBits [GdkButtonPressMask]) 700 500
	gdkWindowShow w
	r <- atomically do
		i@(WindowId i') <- readTVar nid
		writeTVar nid $ WindowId (i' + 1)
		modifyTVar i2w $ insert i w
		modifyTVar w2i $ insert w i
		pure . Just . App.Singleton $ OccWindowNew i
	pure r

handleMouseDown ::
	TVar (Map GdkWindow WindowId) -> Handle' IO (Singleton MouseDown)
handleMouseDown w2i (unSingleton -> MouseDownReq) = do
	getMouseDown w2i

eventButtonToMouseDown :: Map GdkWindow WindowId -> GdkEventButton -> Occurred MouseDown
eventButtonToMouseDown w2i e = OccMouseDown (w2i ! w) $ numToButton b
	where
	w = gdkEventButtonWindow e
	b = gdkEventButtonButton e

numToButton :: CUInt -> MouseBtn
numToButton 1 = ButtonLeft
numToButton 2 = ButtonMiddle
numToButton 3 = ButtonRight
numToButton n = ButtonUnknown $ fromIntegral n

toMouseDown :: Map GdkWindow WindowId -> GdkEvent s -> IO (Maybe (EvOccs (Singleton MouseDown)))
toMouseDown w2i (GdkEventGdkButtonPress e_) = do
	e <- gdkEventButton e_
	pure . Just . App.Singleton $ eventButtonToMouseDown w2i e
toMouseDown _ _ = pure Nothing

getMouseDown :: TVar (Map GdkWindow WindowId) -> IO (Maybe (EvOccs (Singleton MouseDown)))
getMouseDown tw2i = gdkWithEvent \case
	Just e@(GdkEventGdkAny a_) -> do
		a <- gdkEventAny a_
		w2i <- atomically $ readTVar tw2i
		r <- toMouseDown w2i e
		pure r
	Nothing -> threadDelay 100000 >> pure Nothing
