import Data.Maybe
import Data.IORef

import Control.Monad

import Gtk

main :: IO ()
main = do
	pressed <- newIORef False
	xr <- newIORef 60
	yr <- newIORef 60
	fyr <- newIORef (+ 15)
	gtkInit
	w <- gtkWindowNew
	a <- gtkDrawingAreaNew
	gSignalConnect (cast w) "destroy" gtkMainQuit
	gSignalConnect (cast w) "key-press-event" (keyPressed pressed xr)
	gSignalConnect (cast w) "key-release-event" (keyReleased pressed)
	gSignalConnect (cast a) "draw" (drawRect xr yr)
	gtkContainerAdd (cast w) (cast a)
	gtkWidgetShowAll (cast w)
	gTimeoutAddSimple 400 $ do
		y <- readIORef yr
		when (y < 60) $ writeIORef fyr (+ 15)
		when (y > 300) $ writeIORef fyr (subtract 15)
		fy <- readIORef fyr
		modifyIORef yr fy
		gtkWidgetQueueDraw (cast a)
		return True
	gtkMain

drawRect :: IORef Double -> IORef Double -> GtkWidget -> IO ()
drawRect xr yr w = do
	x <- readIORef xr
	y <- readIORef yr
	win <- gtkWidgetGetWindow w
	cr <- gdkCairoCreate (cast win)
	cairoRectangle cr x y 14 14
	cairoRectangle cr (x + 15) y 14 14
	cairoRectangle cr (x + 30) y 14 14
	cairoRectangle cr x (y + 15) 14 14
	cairoRectangle cr 90 300 14 14
	cairoFill cr
	cairoDestroy cr

keyQ, keyH, keyL :: Keyval
[keyQ, keyH, keyL] = map char2keyval "qhl"

keyPressed :: IORef Bool -> IORef Double -> GtkWidget -> GdkEventKey -> IO ()
keyPressed pr xr w e = do
	p <- readIORef pr
	when (not p) $ do
		writeIORef pr True
		kv <- gdkEventKeyGetKeyval e
		modifyIORef xr (fx kv)
		when (kv == keyQ) gtkMainQuit
		gtkWidgetQueueDraw (cast w)
	where
	fx k	| k == keyH = subtract 15
		| k == keyL = (+ 15)
		| otherwise = id

keyReleased :: IORef Bool -> IO ()
keyReleased pr = putStrLn "key released" >> writeIORef pr False

cast :: (GObject g1, GObject g2) => g1 -> g2
cast = fromJust . castGObject
