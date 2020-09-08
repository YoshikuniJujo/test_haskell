{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
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
import Control.Moffy.Event.CalcTextExtents
import Control.Moffy.Handle
import Control.Concurrent
import Control.Concurrent.STM hiding (retry)
import Data.Type.Set
import Data.OneOrMore as Oom
import Data.Int
import Data.Time
import System.Timeout
import Graphics.Gtk as Gtk
import Graphics.Gtk.Cairo
import Graphics.Gtk.Pango

import Control.Moffy.Event.Time
import Control.Moffy.Handle.Time
import Data.Time.Clock.TAI

import qualified Data.Text as T

import Data.Bool

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

class Drawable a where
	draw :: GtkWidget -> CairoT -> a -> IO ()

instance Drawable () where draw _ _ () = pure ()

instance Drawable a => Drawable [a] where
	draw w cr xs = draw w cr `mapM_` reverse xs

instance Drawable Double where
	draw w cr n = do
		cairoMoveTo cr 200 100
		cairoLineTo cr (210 + 10 * n) (110 + 10 * n)
		cairoStroke cr

type GuiEv = CalcTextExtents :- M.DeleteEvent :- KeyEv :+: MouseEv

tryUseTChan :: (Drawable a, Monoid a) => IO (TChan (EvReqs GuiEv), TChan (EvOccs GuiEv), TChan a)
tryUseTChan = do

	cr <- newTChanIO
	c <- newTChanIO
	c' <- newTChanIO
	void . forkIO $ allocaMutable \m -> do
		tx <- atomically $ newTVar mempty

		ftc <- newTChanIO
--		pokeMutable m =<< newArr []
		[] <- gtkInit []
		w <- gtkWindowNew gtkWindowToplevel

		gtkWidgetSetEvents w [gdkPointerMotionMask]
--		gtkWidgetSetEvents w [gdkButtonMotionMask]
		gSignalConnect w DeleteEvent (\a b x' -> True <$ (print' (a, b, x') >>
			atomically (writeTChan c . Oom.expand $ Singleton OccDeleteEvent))) ()
		gSignalConnect w KeyPressEvent (\a b x' -> False <$ (print' (a, b, x') >> do
			e <- gdkEventKeyToOccKeyDown b
			atomically (writeTChan c $ Oom.expand e))) ()
		gSignalConnect w KeyReleaseEvent (\a b x' -> False <$ (print' (a, b, x') >> do
			e <- gdkEventKeyToOccKeyUp b
			atomically (writeTChan c $ Oom.expand e))) ()
		gSignalConnect w ButtonPressEvent (\a b x' -> True <$ (print' (a, b, x') >> do
			e <- gdkEventButtonToOccMouseDown b
			atomically (writeTChan c $ Oom.expand e))) ()
		gSignalConnect w ButtonReleaseEvent (\a b x' -> True <$ (print' (a, b, x') >> do
			e <- gdkEventButtonToOccMouseUp b
			atomically (writeTChan c $ Oom.expand e))) ()
		gSignalConnect w MotionNotifyEvent (\a b x' -> True <$ do
			print' (a, b, x')
			e <- gdkEventMotionToOccMouseMove b
			atomically (writeTChan c $ Oom.expand e)) ()
		gSignalConnect w Destroy gtkMainQuit ()

		da <- gtkDrawingAreaNew
		gtkContainerAdd (castWidgetToContainer w) da
		gSignalConnect da DrawEvent (tryDraw ftc c tx) m

		void . flip (gTimeoutAdd 101) () $ const do
			atomically (lastTChan cr) >>= \case
				Nothing -> pure True
				Just rqs -> do
--					putStrLn "here"
					case project rqs of
						Nothing -> pure True
--						Just (TextExtents xb yb tw th xa ya) -> do
						Just (CalcTextExtentsReq fn fs t) -> do
							putStrLn "CalcTextExtents"
							print (fn, fs, t)
							atomically $ writeTChan ftc (fn, fs, t)
							gtkWidgetQueueDraw da
							pure True

		void . flip (gTimeoutAdd 100) () $ const do
			atomically (lastTChan c') >>= \case
				Nothing -> pure True
				Just v -> do
					atomically $ writeTVar tx v
					gtkWidgetQueueDraw da
					pure True

		gtkWidgetShowAll w
		gtkMain
	pure (cr, c, c')

lastTChan :: TChan a -> STM (Maybe a)
lastTChan c = do
	tryReadTChan c >>= \case
		Nothing -> pure Nothing
		Just x -> bool (lastTChan c) (pure $ Just x) =<< isEmptyTChan c

lastTChan' :: a -> TChan a -> STM a
lastTChan' x c = do
	mx' <- tryReadTChan c
	maybe (pure x) (`lastTChan'` c) mx'

tryDraw :: Drawable a =>
	TChan (FontName, FontSize, T.Text) -> TChan (EvOccs GuiEv) -> TVar a -> GtkWidget -> CairoT -> Mutable (Arr a) -> IO Bool
tryDraw ftc co tx w cr x = True <$ do
	m3 <- atomically $ lastTChan ftc
	case m3 of
		Just (fn, fs, txt) -> do
			cairoWithTextExtents cr txt $ \e -> do
				print =<< cairoTextExtentsXBearing e
				print =<< cairoTextExtentsYBearing e
				print =<< cairoTextExtentsWidth e
				print =<< cairoTextExtentsHeight e
			l <- pangoCairoCreateLayout cr
			pangoLayoutSetText l txt
			d <- pangoFontDescriptionFromString $ T.pack fn
			pangoFontDescriptionSetAbsoluteSize d fs
			pangoLayoutSetFontDescription l d
			te <- pangoLayoutWithPixelExtents l \ie le -> do
				putStrLn "ink_rect"
				print =<< pangoRectangleX ie
				print =<< pangoRectangleY ie
				print =<< pangoRectangleWidth ie
				print =<< pangoRectangleHeight ie
				putStrLn "logical_rect"
				print =<< pangoRectangleX le
				print =<< pangoRectangleY le
				print =<< pangoRectangleWidth le
				print =<< pangoRectangleHeight le
				mkTextExtents ie le
			atomically . writeTChan co . Oom.expand . Singleton
				$ OccCalcTextExtents fn fs txt te
		Nothing -> pure ()
	draw w cr =<< atomically (readTVar tx)
--	draw cr =<< peekArr =<< peekMutable x

rectangle :: Int32 -> Int32 -> Int32 -> Int32 -> Rectangle
rectangle (fromIntegral -> l) (fromIntegral -> t) (fromIntegral -> w) (fromIntegral -> h) =
	Rectangle l t w h

mkTextExtents :: PangoRectangle -> PangoRectangle -> IO TextExtents'
mkTextExtents ie le = TextExtents'
	<$> (rectangle <$> pangoRectangleX ie <*> pangoRectangleY ie <*> pangoRectangleWidth ie <*> pangoRectangleHeight ie)
	<*> (rectangle <$> pangoRectangleX le <*> pangoRectangleY le <*> pangoRectangleWidth le <*> pangoRectangleHeight le)

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
	x <- gdkEventMotionX e
	y <- gdkEventMotionY e
	pure $ Singleton $ OccMouseMove (x, y)

gdkEventButtonToOccMouseDown :: GdkEventButton -> IO (EvOccs (MouseDown :- MouseMove :- 'Nil))
gdkEventButtonToOccMouseDown e = do
	x <- gdkEventButtonX e
	y <- gdkEventButtonY e
	b <- gdkEventButtonButton e
	pure $ OccMouseDown (btn b) >- Singleton (OccMouseMove (x, y))
	where
	btn = \case
		1 -> ButtonLeft; 2 -> ButtonMiddle; 3 -> ButtonRight
		n -> ButtonUnknown n

gdkEventButtonToOccMouseUp :: GdkEventButton -> IO (EvOccs (MouseUp :- MouseMove :- 'Nil))
gdkEventButtonToOccMouseUp e = do
	x <- gdkEventButtonX e
	y <- gdkEventButtonY e
	b <- gdkEventButtonButton e
	pure $ OccMouseUp (btn b) >- Singleton (OccMouseMove (x, y))
	where
	btn = \case
		1 -> ButtonLeft; 2 -> ButtonMiddle; 3 -> ButtonRight
		n -> ButtonUnknown n

handleDelete :: Maybe DiffTime -> TChan (EvReqs GuiEv) -> TChan (EvOccs GuiEv) -> Handle' IO GuiEv
-- handleDelete :: DiffTime -> TChan (EvOccs GuiEv) -> Handle' IO GuiEv
handleDelete mt cr c rqs = maybe (Just <$>) (timeout . round . (* 1000000)) mt do
	atomically $ writeTChan cr rqs
	atomically $ readTChan c

-- uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
-- uncurry3 f (x, y, z) = f x y z

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f x y z = f (x, y, z)

handleBoxesFoo :: DiffTime -> TChan (EvReqs GuiEv) -> TChan (EvOccs GuiEv) ->
	HandleSt (Mode, AbsoluteTime) IO (TimeEv :+: GuiEv)
handleBoxesFoo dt cr co = retrySt
	$ ((\f x y z -> f (x, (y, z))) . popInput . handleTimeEvPlus . pushInput) (\(x, (y, z)) -> (((liftHandle' .) .) . handleDelete . Just) x y z) dt cr co
