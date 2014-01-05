import Control.Monad

import Data.Maybe
import Data.IORef

import Gtk

main :: IO ()
main = do
	gtkInit
	xr <- newIORef 0
	fxr <- newIORef id
	yr <- newIORef 0
	fyr <- newIORef (+ 1)
	w <- gtkWindowNew
	a <- gtkDrawingAreaNew
	gSignalConnect (cast w) "destroy" gtkMainQuit
	gSignalConnect (cast w) "key-press-event" (keyPressed xr fxr)
	gSignalConnect (cast w) "key-release-event" (keyReleased fxr)
	gSignalConnect (cast a) "draw" (drawRect xr yr)
	gtkContainerAdd (cast w) (cast a)
	gtkWidgetShowAll (cast w)
	gTimeoutAddSimple 100 $ do
		x <- readIORef xr
		fx <- readIORef fxr
		when (fx x < 0) $ writeIORef fxr id
		when (fx x > 300) $ writeIORef fxr id
		fx <- readIORef fxr
		modifyIORef xr fx
		gtkWidgetQueueDraw (cast a)
		return True
	gTimeoutAddSimple 20 $ do
		y <- readIORef yr
		when (y < 0) $ writeIORef fyr (+ 1)
		when (y > 200) $ writeIORef fyr (subtract 1)
		fy <- readIORef fyr
		modifyIORef yr fy
		gtkWidgetQueueDraw (cast a)
		return True
	gtkMain

cast :: (GObject g1, GObject g2) => g1 -> g2
cast = fromJust . castGObject

drawRect :: IORef Double -> IORef Double -> GtkWidget -> IO ()
drawRect xr yr w = do
	win <- gtkWidgetGetWindow w
	cr <- gdkCairoCreate (cast win)
	x <- readIORef xr
	y <- readIORef yr
	cairoTranslate cr x y
	cairoRectangle cr 60 60 20 20
	cairoFill cr
	cairoDestroy cr
	cr <- gdkCairoCreate (cast win)
	cairoRectangle cr 100 200 20 20
	cairoFill cr
	cairoDestroy cr

keyQ, keyH, keyJ, keyK, keyL :: Keyval
[keyQ, keyH, keyJ, keyK, keyL] = map char2keyval "qhjkl"

keyPressed :: IORef Double -> IORef (Double -> Double) -> GtkWidget -> GdkEventKey -> IO ()
keyPressed xr fxr _w e = do
	print e
	kv <- gdkEventKeyGetKeyval e
	writeIORef fxr (fx kv)
	fx <- readIORef fxr
--	modifyIORef xr fx
--	x <- readIORef xr
--	modifyIORef xr (fx kv x)
	print =<< gdkEventKeyGetKeyval e
	print =<< gdkEventKeyGetString e
	when (kv == keyQ) gtkMainQuit
	where
	fx k	| k == keyH = subtract 20
		| k == keyL = (+ 20)
		| otherwise = id

keyReleased :: IORef (Double -> Double) -> GtkWidget -> GdkEventKey -> IO ()
keyReleased fxr _w e = do
	kv <- gdkEventKeyGetKeyval e
	when (kv == keyH || kv == keyL) $
		writeIORef fxr id
