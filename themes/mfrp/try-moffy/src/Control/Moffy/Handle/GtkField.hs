{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Handle.GtkField where

import Prelude hiding (repeat, break)

import Control.Monad
import Control.Moffy
import Control.Moffy.Run
import Control.Moffy.Event.Delete as M
import Control.Moffy.Event.Key
import Control.Moffy.Event.Mouse
import Control.Moffy.Handle
import Control.Concurrent
import Control.Concurrent.STM hiding (retry)
import Data.Type.Set
import Data.OneOrMore as Oom
import Data.Time
import System.Timeout
import Graphics.Gtk as Gtk
import Graphics.Gtk.Cairo
import Graphics.Gtk.Values

import Control.Moffy.Event.Time
import Control.Moffy.Handle.Time
import Data.Time.Clock.TAI
import Foreign.Storable

import Arr

tryGtk :: IO ()
tryGtk = do
	[] <- gtkInit []
	w <- gtkWindowNew gtkWindowToplevel
	gtkWidgetShowAll w
	gSignalConnect w Destroy gtkMainQuit ()
	gtkMain

print' :: a -> IO ()
print' _ = pure ()

class Drawable a where draw :: CairoT -> a -> IO ()

instance Drawable a => Drawable [a] where draw cr xs = draw cr `mapM_` xs

instance Drawable Double where
	draw cr n = do
		cairoMoveTo cr 200 100
		cairoLineTo cr (210 + 10 * n) (110 + 10 * n)
		cairoStroke cr

tryUseTChan :: (Show a, Storable a, Drawable a) => IO (TChan (EvOccs (M.DeleteEvent :- KeyEv :+: MouseEv)), TChan [a])
tryUseTChan = allocaMutable \m -> do
	pokeMutable m =<< newArr []
	c <- newTChanIO
	c' <- newTChanIO
	void . forkIO $ do
		[] <- gtkInit []
		w <- gtkWindowNew gtkWindowToplevel

--		gtkWidgetSetEvents w [gdkPointerMotionMask]
		gtkWidgetSetEvents w [gdkButtonMotionMask]
		gSignalConnect w DeleteEvent (\a b c' -> True <$ (print' (a, b, c') >>
			atomically (writeTChan c . Oom.expand $ Singleton OccDeleteEvent))) ()
		gSignalConnect w KeyPressEvent (\a b c' -> False <$ (print' (a, b, c') >> do
			e <- gdkEventKeyToOccKeyDown b
			atomically (writeTChan c $ Oom.expand e))) ()
		gSignalConnect w KeyReleaseEvent (\a b c' -> False <$ (print' (a, b, c') >> do
			e <- gdkEventKeyToOccKeyUp b
			atomically (writeTChan c $ Oom.expand e))) ()
		gSignalConnect w ButtonPressEvent (\a b c' -> True <$ (print' (a, b, c') >> do
--			pokeMutable m . (+ 1) =<< peekMutable m
			e <- gdkEventButtonToOccMouseDown b
			atomically (writeTChan c $ Oom.expand e))) ()
		gSignalConnect w ButtonReleaseEvent (\a b c' -> True <$ (print' (a, b, c') >> do
			e <- gdkEventButtonToOccMouseUp b
			atomically (writeTChan c $ Oom.expand e))) ()
		gSignalConnect w MotionNotifyEvent (\a b c' -> True <$ (print' (a, b, c') >> do
			e <- gdkEventMotionToOccMouseMove b
			atomically (writeTChan c $ Oom.expand e))) ()
		gSignalConnect w Destroy gtkMainQuit ()

		da <- gtkDrawingAreaNew
		gtkContainerAdd (castWidgetToContainer w) da
--		gSignalConnect da DrawEvent (\a b c' -> False <$ print (a, b, c')) ()
		gSignalConnect da DrawEvent tryDraw m

		forkIO . forever $ do
			v <- atomically $ readTChan c'
			print v
			old <- peekMutable m
--			freeArr =<< peekMutable m
			pokeMutable m =<< newArr v
			freeArr old
			gtkWidgetQueueDraw da

		gtkWidgetShowAll w
		gtkMain
	pure (c, c')

tryDraw :: (Storable a, Drawable a) => GtkWidget -> CairoT -> Mutable (Arr a) -> IO Bool
tryDraw w cr x = True <$ do
	print (w, cr, x)
--	print =<< peekMutable x
	cairoMoveTo cr 100 100
	cairoLineTo cr 500 400
	cairoStroke cr
	draw cr =<< peekArr =<< peekMutable x

gdkEventKeyToOccKeyDown :: GdkEventKey -> IO (EvOccs (Singleton KeyDown))
gdkEventKeyToOccKeyDown e = do
	kv <- keyval e
	pure . Singleton . OccKeyDown . Key $ fromIntegral kv

gdkEventKeyToOccKeyUp :: GdkEventKey -> IO (EvOccs (Singleton KeyUp))
gdkEventKeyToOccKeyUp e = do
	kv <- keyval e
	pure . Singleton . OccKeyUp . Key $ fromIntegral kv

gdkEventMotionToOccMouseMove :: GdkEventMotion -> IO (EvOccs (Singleton MouseMove))
gdkEventMotionToOccMouseMove e = do
	x <- round <$> gdkEventMotionX e
	y <- round <$> gdkEventMotionY e
	pure $ Singleton $ OccMouseMove (x, y)

gdkEventButtonToOccMouseDown :: GdkEventButton -> IO (EvOccs (MouseDown :- MouseMove :- 'Nil))
gdkEventButtonToOccMouseDown e = do
	x <- round <$> gdkEventButtonX e
	y <- round <$> gdkEventButtonY e
	b <- gdkEventButtonButton e
	pure $ OccMouseDown (btn b) >- Singleton (OccMouseMove (x, y))
	where
	btn = \case
		1 -> ButtonLeft; 2 -> ButtonMiddle; 3 -> ButtonRight
		n -> ButtonUnknown n

gdkEventButtonToOccMouseUp :: GdkEventButton -> IO (EvOccs (MouseUp :- MouseMove :- 'Nil))
gdkEventButtonToOccMouseUp e = do
	x <- round <$> gdkEventButtonX e
	y <- round <$> gdkEventButtonY e
	b <- gdkEventButtonButton e
	pure $ OccMouseUp (btn b) >- Singleton (OccMouseMove (x, y))
	where
	btn = \case
		1 -> ButtonLeft; 2 -> ButtonMiddle; 3 -> ButtonRight
		n -> ButtonUnknown n

{-
runUseTChan :: IO ()
runUseTChan = do
	(c, c') <- tryUseTChan
	atomically (readTChan c)
	gtkMainQuit
	-}

handleDelete :: DiffTime -> TChan (EvOccs (M.DeleteEvent :- KeyEv :+: MouseEv)) -> Handle' IO (M.DeleteEvent :- KeyEv :+: MouseEv)
handleDelete t c _rqs = timeout (round $ t * 1000000) . atomically $ readTChan c

{-
runHandleDelete :: IO ()
runHandleDelete = do
	(c, c') <- tryUseTChan
	interpret (retry $ handleDelete 0.1 c) print
		$ repeat (keyDown `first` keyUp `first` mouseMove `first` mouseDown `first` mouseUp)  `break` deleteEvent
	gtkMainQuit
	-}

handleBoxesFoo :: DiffTime -> TChan (EvOccs (M.DeleteEvent :- KeyEv :+: MouseEv)) ->
	HandleSt (Mode, AbsoluteTime) IO (TimeEv :+: M.DeleteEvent :- KeyEv :+: MouseEv)
handleBoxesFoo = ((retrySt .) .) . curry . popInput . handleTimeEvPlus
	. pushInput . uncurry $ (liftHandle' .) . handleDelete
