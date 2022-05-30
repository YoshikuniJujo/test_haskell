{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TryGdk.Boxes where

import Prelude hiding (break)

import Foreign.C.Types
import Control.Concurrent
import Control.Concurrent.STM
import Data.Type.Flip
import Data.Type.Set
import Data.OneOrMore
import qualified Data.OneOrMoreApp as App
import Data.Maybe
import Data.Map (Map, empty, insert, (!))
import Data.Time
import Data.Time.Clock.TAI
import Data.Time.Clock.System
import Data.Color

import Control.Moffy
import Control.Moffy.Event.Time
import Control.Moffy.Event.Window
import Control.Moffy.Event.DefaultWindow
import Control.Moffy.Event.Delete (pattern OccDeleteEvent)
import Control.Moffy.Event.Delete.DefaultWindow
import Control.Moffy.Event.Mouse
import Control.Moffy.Event.Key
import Control.Moffy.Handle as H
import Control.Moffy.Handle.Time
import Control.Moffy.Handle.DefaultWindow
import Control.Moffy.Run
import Control.Moffy.Viewable.Shape

import Data.CairoContext
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths

import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.Windows
import Graphics.Gdk.Windows.GdkWindowAttr
import Graphics.Gdk.Windows.GdkEventMask
import Graphics.Gdk.Events
import Graphics.Gdk.EventStructures
import Graphics.Gdk.GdkDrawingContext

import Trial.Boxes.BoxEv

showBoxes :: GdkWindow -> [Box] -> IO ()
showBoxes w bs = do
	rgn <- gdkWindowGetVisibleRegion w
	gdkWindowWithDrawFrame w rgn \ctx -> do
		cr <- gdkDrawingContextGetCairoContext ctx
		drawBox cr `mapM_` bs

showBox :: GdkWindow -> Box -> IO ()
showBox w b = do
	rgn <- gdkWindowGetVisibleRegion w
	gdkWindowWithDrawFrame w rgn \ctx -> do
		cr <- gdkDrawingContextGetCairoContext ctx
		drawBox cr b

drawBox :: CairoTIO s -> Box -> IO ()
drawBox cr (Box (Rect (l_, u_) (r_, d_)) bc) = do
	let (l, u, r, d) = lurd l_ u_ r_ d_
	cairoSetSourceRgb cr $ bColorToRgb bc
	cairoRectangle cr l u (r - l) (d - u)
	cairoFill cr

showRect :: GdkWindow -> Rect -> IO ()
showRect w (Rect (l_, u_) (r_, d_)) = do
	let (l, u, r, d) = lurd l_ u_ r_ d_
	rgn <- gdkWindowGetVisibleRegion w
	gdkWindowWithDrawFrame w rgn \ctx -> do
		cr <- gdkDrawingContextGetCairoContext ctx
		cairoSetSourceRgb cr . fromJust $ rgbDouble 0 0.5 0
		cairoRectangle cr l u (r - l) (d - u)
		cairoFill cr

lurd :: Double -> Double -> Double -> Double ->
	(CDouble, CDouble, CDouble, CDouble)
lurd l u r d = (
		realToFrac $ l `min` r, realToFrac $ u `min` d,
		realToFrac $ l `max` r, realToFrac $ u `max` d )

showColor :: GdkWindow -> BColor -> IO ()
showColor w bc = do
	r <- gdkWindowGetVisibleRegion w
	gdkWindowWithDrawFrame w r \ctx -> do
		cr <- gdkDrawingContextGetCairoContext ctx
		cairoSetSourceRgb cr $ bColorToRgb bc
		cairoPaint cr

bColorToRgb :: BColor -> Rgb CDouble
bColorToRgb Red = fromJust $ rgbDouble 0.5 0 0
bColorToRgb Green = fromJust $ rgbDouble 0 0.5 0
bColorToRgb Blue = fromJust $ rgbDouble 0 0 0.5
bColorToRgb Yellow = fromJust $ rgbDouble 0.5 0.5 0
bColorToRgb Cyan = fromJust $ rgbDouble 0 0.5 0.5
bColorToRgb Magenta = fromJust $ rgbDouble 0.5 0 0.5

tryGdk :: (Show a, Show r) => (GdkWindow -> a -> IO ()) -> Sig s TryGdkEv a r -> IO ()
tryGdk vw sg = do
	(wid, i2w, w2i, t) <- initialize
	(print =<<) . ($ initTryGdkState t)
		$ interpretSt @_ @TryGdkEv (handle wid i2w w2i)
			(viewGdk vw i2w)
			(sigGdk sg `break` deleteEvent)

viewGdk :: (GdkWindow -> a -> IO ()) ->
	TVar (Map WindowId GdkWindow) -> (WindowId, a) -> IO ()
viewGdk vw i2w (i, x) = do
	w <- (! i) <$> atomically (readTVar i2w)
	vw w x

sigGdk :: Sig s TryGdkEv a r -> Sig s TryGdkEv (WindowId, a) r
sigGdk sg = do
	w <- adjustSig defaultWindowNew
	(w ,) <$%> sg

-- type TryGdkEv = WindowEv :+: DefaultWindowEv :+: MouseDown :- MouseUp :- MouseMove :- KeyEv :+: TimeEv :+: DeleteEvent :- 'Nil
type TryGdkEv = BoxEv -- WindowEv :+: DefaultWindowEv :+: MouseDown :- MouseUp :- MouseMove :- KeyEv :+: TimeEv :+: DeleteEvent :- 'Nil

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
handle wid i2w w2i = retrySt $ handleGdk' wid i2w w2i (0.05, ()) `mergeSt` handleDefaultWindow

handleGdk' :: TimeState s =>
	TVar WindowId ->
	TVar (Map WindowId GdkWindow) -> TVar (Map GdkWindow WindowId) -> (DiffTime, ()) ->
	HandleSt' s IO (WindowEv :+: MouseDown :- MouseUp :- MouseMove :- MouseScroll :- KeyEv :+: TimeEv :+: DeleteEvent :- 'Nil)
handleGdk' wid i2w w2i = popInput . handleTimeEvPlus . pushInput . const . liftHandle' $ handleGdk wid i2w w2i

handleGdk ::
	TVar WindowId ->
	TVar (Map WindowId GdkWindow) -> TVar (Map GdkWindow WindowId) ->
	Handle' IO (WindowEv :+: MouseDown :- MouseUp :- MouseMove :- MouseScroll :- KeyEv :+: DeleteEvent :- 'Nil)
handleGdk wid i2w w2i rqs = do
	H.expand @_ @_ @WindowEv (handleWindowNew wid i2w w2i) `H.merge` handleMouseDown w2i $ rqs

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

handleMouseDown ::
	TVar (Map GdkWindow WindowId) -> Handle' IO (MouseDown :- MouseUp :- MouseMove :- MouseScroll :- KeyEv :+: DeleteEvent :- 'Nil)
handleMouseDown w2i _ = do
	getMouseDown w2i

eventButtonToMouseDown :: Map GdkWindow WindowId -> GdkEventButton -> Occurred MouseDown
eventButtonToMouseDown w2i e = OccMouseDown (w2i ! w) $ numToButton b
	where
	w = gdkEventButtonWindow e
	b = gdkEventButtonButton e

eventButtonToMouseUp :: Map GdkWindow WindowId -> GdkEventButton -> Occurred MouseUp
eventButtonToMouseUp w2i e = OccMouseUp (w2i ! w) $ numToButton b
	where
	w = gdkEventButtonWindow e
	b = gdkEventButtonButton e

eventButtonToMouseMove :: Map GdkWindow WindowId -> GdkEventButton -> Occurred MouseMove
eventButtonToMouseMove w2i e = OccMouseMove (w2i ! w) (x, y)
	where
	w = gdkEventButtonWindow e
	x = realToFrac $ gdkEventButtonX e
	y = realToFrac $ gdkEventButtonY e

eventMotionToMouseMove :: Map GdkWindow WindowId -> GdkEventMotion -> Occurred MouseMove
eventMotionToMouseMove w2i e = OccMouseMove (w2i ! w) (x, y)
	where
	w = gdkEventMotionWindow e
	x = realToFrac $ gdkEventMotionX e
	y = realToFrac $ gdkEventMotionY e

numToButton :: CUInt -> MouseBtn
numToButton 1 = ButtonLeft
numToButton 2 = ButtonMiddle
numToButton 3 = ButtonRight
numToButton n = ButtonUnknown $ fromIntegral n

toMouseDown, toMouseUp :: Map GdkWindow WindowId -> GdkEventButton ->
	Maybe (EvOccs (MouseDown :- MouseUp :- MouseMove :- MouseScroll :- KeyEv :+: DeleteEvent :- 'Nil))
toMouseDown w2i e =
	Just $ App.expand
		(eventButtonToMouseMove w2i e App.>- App.Singleton (eventButtonToMouseDown w2i e) :: EvOccs (MouseDown :- MouseMove :- 'Nil))

toMouseUp w2i e =
	Just $ App.expand
		(eventButtonToMouseMove w2i e App.>- App.Singleton (eventButtonToMouseUp w2i e) :: EvOccs (MouseUp :- MouseMove :- 'Nil))

toMouseMove :: Map GdkWindow WindowId -> GdkEventMotion -> Maybe (EvOccs (MouseDown :- MouseUp :- MouseMove :- MouseScroll :- KeyEv :+: DeleteEvent :- 'Nil))
toMouseMove w2i e =
	Just $ App.expand
		(App.Singleton $ eventMotionToMouseMove w2i e)

getMouseDown :: TVar (Map GdkWindow WindowId) -> IO (Maybe (EvOccs (MouseDown :- MouseUp :- MouseMove :- MouseScroll :- KeyEv :+: DeleteEvent :- 'Nil)))
getMouseDown tw2i = gdkWithEvent \case
	Just (GdkEventGdkDelete e_) -> do
		e <- gdkEventAny e_
		w2i <- atomically $ readTVar tw2i
		let	w = w2i ! gdkEventAnyWindow e
		pure . Just . App.expand . App.Singleton $ OccDeleteEvent w
	Just (GdkEventGdkButtonPress e_) -> do
		e <- gdkEventButton e_
		w2i <- atomically $ readTVar tw2i
		pure $ toMouseDown w2i e
	Just (GdkEventGdkButtonRelease e_) -> do
		e <- gdkEventButton e_
		w2i <- atomically $ readTVar tw2i
		pure $ toMouseUp w2i e
	Just (GdkEventGdkMotionNotify e_) -> do
		e <- gdkEventMotion e_
		w2i <- atomically $ readTVar tw2i
		pure $ toMouseMove w2i e
	Just e@(GdkEventGdkAny a_) -> do
		a <- gdkEventAny a_
		pure Nothing
	Nothing -> threadDelay 50000 >> pure Nothing
