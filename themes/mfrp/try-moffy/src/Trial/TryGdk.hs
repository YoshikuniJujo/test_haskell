{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TryGdk where

import Foreign.C.Types
import Control.Concurrent
import Control.Concurrent.STM
import Data.Type.Flip
import Data.Type.Set
import Data.OneOrMore
import qualified Data.OneOrMoreApp as App
import Data.Map (Map, empty, insert, (!))
import Data.Time
import Data.Time.Clock.TAI
import Data.Time.Clock.System

import Control.Moffy
import Control.Moffy.Event.Time
import Control.Moffy.Event.Window
import Control.Moffy.Event.DefaultWindow
import Control.Moffy.Event.Mouse
import Control.Moffy.Handle as H
import Control.Moffy.Handle.Time
import Control.Moffy.Handle.DefaultWindow
import Control.Moffy.Run

import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.Windows
import Graphics.Gdk.Windows.GdkWindowAttr
import Graphics.Gdk.Windows.GdkEventMask
import Graphics.Gdk.Events
import Graphics.Gdk.EventStructures

tryGdk :: Show r => (GdkWindow -> a -> IO ()) -> Sig s TryGdkEv a r -> IO ()
tryGdk vw sg = do
	(wid, i2w, w2i, t) <- initialize
	(print =<<) . ($ initTryGdkState t)
		$ interpretSt @_ @TryGdkEv (handle wid i2w w2i)
			(viewGdk vw i2w)
			(sigGdk sg)

viewGdk :: (GdkWindow -> a -> IO ()) ->
	TVar (Map WindowId GdkWindow) -> (WindowId, a) -> IO ()
viewGdk vw i2w (i, x) = do
	w <- (! i) <$> atomically (readTVar i2w)
	vw w x

sigGdk :: Sig s TryGdkEv a r -> Sig s TryGdkEv (WindowId, a) r
sigGdk sg = do
	w <- adjustSig defaultWindowNew
	(w ,) <$%> sg

type TryGdkEv = WindowNew :- DefaultWindowEv :+: MouseDown :- MouseMove :- TimeEv

initialize :: IO (
	TVar WindowId,
	TVar (Map WindowId GdkWindow), TVar (Map GdkWindow WindowId),
	AbsoluteTime )
initialize = do
	gdkDisplayOpen ""
	wid <- atomically . newTVar $ WindowId 0
	i2w <- atomically $ newTVar empty
	w2i <- atomically $ newTVar empty
	t <- getTAITime
	pure (wid, i2w, w2i, t)

defaultWindowNew :: Sig s (WindowNew :- StoreDefaultWindow :- 'Nil) a WindowId
defaultWindowNew = do
	w <- waitFor $ adjust windowNew
	w <$ waitFor (adjust $ storeDefaultWindow w)

data TryGdkState = TryGdkState {
	windowId :: Maybe WindowId,
	timeMode :: Mode,
	lastTime :: AbsoluteTime } deriving Show

instance DefaultWindowState TryGdkState where
	getDefaultWindow = windowId
	putDefaultWindow s wid = s { windowId = Just wid }
	
instance TimeState TryGdkState where
	getMode = timeMode
	putMode s m = s { timeMode = m }
	getLatestTime = lastTime
	putLatestTime s t = s { lastTime = t }

initTryGdkState :: AbsoluteTime -> TryGdkState
initTryGdkState = TryGdkState Nothing InitialMode

getTAITime :: IO AbsoluteTime
getTAITime = systemToTAITime <$> getSystemTime

handle :: TVar WindowId -> TVar (Map WindowId GdkWindow) -> TVar (Map GdkWindow WindowId) ->
	HandleSt TryGdkState IO TryGdkEv
handle wid i2w w2i = retrySt $ handleGdk' wid i2w w2i (0.1, ()) `mergeSt` handleDefaultWindow

handleGdk' :: TimeState s =>
	TVar WindowId ->
	TVar (Map WindowId GdkWindow) -> TVar (Map GdkWindow WindowId) -> (DiffTime, ()) ->
	HandleSt' s IO (WindowNew :- MouseDown :- MouseMove :- TimeEv)
handleGdk' wid i2w w2i = popInput . handleTimeEvPlus . pushInput . const . liftHandle' $ handleGdk wid i2w w2i

handleGdk ::
	TVar WindowId ->
	TVar (Map WindowId GdkWindow) -> TVar (Map GdkWindow WindowId) ->
	Handle' IO (WindowNew :- MouseDown :- MouseMove :- 'Nil)
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
	TVar (Map GdkWindow WindowId) -> Handle' IO (MouseDown :- MouseMove :- 'Nil)
handleMouseDown w2i _ = do
	getMouseDown w2i

eventButtonToMouseDown :: Map GdkWindow WindowId -> GdkEventButton -> Occurred MouseDown
eventButtonToMouseDown w2i e = OccMouseDown (w2i ! w) $ numToButton b
	where
	w = gdkEventButtonWindow e
	b = gdkEventButtonButton e

eventButtonToMouseMove :: Map GdkWindow WindowId -> GdkEventButton -> Occurred MouseMove
eventButtonToMouseMove w2i e = OccMouseMove (w2i ! w) (x, y)
	where
	w = gdkEventButtonWindow e
	x = realToFrac $ gdkEventButtonX e
	y = realToFrac $ gdkEventButtonY e

numToButton :: CUInt -> MouseBtn
numToButton 1 = ButtonLeft
numToButton 2 = ButtonMiddle
numToButton 3 = ButtonRight
numToButton n = ButtonUnknown $ fromIntegral n

toMouseDown :: Map GdkWindow WindowId -> GdkEvent s -> IO (Maybe (EvOccs (MouseDown :- MouseMove :- 'Nil)))
toMouseDown w2i (GdkEventGdkButtonPress e_) = do
	e <- gdkEventButton e_
	pure . Just $ eventButtonToMouseMove w2i e App.>- App.Singleton (eventButtonToMouseDown w2i e)
toMouseDown _ _ = pure Nothing

getMouseDown :: TVar (Map GdkWindow WindowId) -> IO (Maybe (EvOccs (MouseDown :- MouseMove :- 'Nil)))
getMouseDown tw2i = gdkWithEvent \case
	Just e@(GdkEventGdkAny a_) -> do
		a <- gdkEventAny a_
		w2i <- atomically $ readTVar tw2i
		r <- toMouseDown w2i e
		pure r
	Nothing -> threadDelay 100000 >> pure Nothing
