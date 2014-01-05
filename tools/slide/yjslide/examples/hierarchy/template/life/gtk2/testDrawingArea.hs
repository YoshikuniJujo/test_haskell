import Control.Monad

import Data.Maybe
import Data.IORef

import Gtk

main :: IO ()
main = do
	gtkInit
	x <- newIORef 0
	w <- gtkWindowNew
	a <- gtkDrawingAreaNew
	gSignalConnect (cast w) "destroy" gtkMainQuit
	gSignalConnect (cast w) "key-press-event" (keyPressed x)
	gSignalConnect (cast a) "draw" (drawRect x)
	gtkContainerAdd (cast w) (cast a)
	gtkWidgetShowAll (cast w)
	gTimeoutAddSimple 100 $ do
		xx <- readIORef x
		when (xx < 200) $ modifyIORef x (+ 1)
		gtkWidgetQueueDraw (cast a)
		return True
	gtkMain

cast :: (GObject g1, GObject g2) => g1 -> g2
cast = fromJust . castGObject

drawRect :: IORef Double -> GtkWidget -> IO ()
drawRect xr w = do
	win <- gtkWidgetGetWindow w
	cr <- gdkCairoCreate (cast win)
	x <- readIORef xr
	cairoTranslate cr x x
	cairoRectangle cr 50 50 50 50
	cairoFill cr
	cairoDestroy cr

keyPressed :: IORef Double -> GtkWidget -> GdkEventKey -> IO ()
keyPressed x _w e = do
	modifyIORef x (subtract 1)
	print e
	kv <- gdkEventKeyGetKeyval e
	print =<< gdkEventKeyGetKeyval e
	when (kv == char2keyval 'q') gtkMainQuit
