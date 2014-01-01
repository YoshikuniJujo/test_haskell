import Data.Maybe
import Foreign.Ptr
import Control.Monad

import Gtk
import GObject

main :: IO ()
main = do
	gtkInit
	w <- gtkWindowNew
	b <- gtkDrawingAreaNew
	gtkContainerAdd (cast w) (cast b)
	gtkWidgetShowAll (cast w)
	gSignalConnect (cast w) "destroy" gtkMainQuit
	gSignalConnect (cast b) "button-press-event" gtkMainQuit
	gSignalConnect (cast b) "expose_event" buttonClicked
	print =<< gtkWidgetState (cast w)
	gtkMain

cast :: (GObject g1, GObject g2) => g1 -> g2
cast = fromJust . castGObject

buttonClicked :: GtkWidget -> Ptr () -> IO ()
buttonClicked gw ud = do
	print =<< gtkWidgetState gw
	state <- gtkWidgetState gw
	style <- gtkStyle gw
	gc <- (fgGC (cast style) state)
	gdkWin <- gdkWindow gw
	forM_ [0 .. 400] $ \x ->
		gdkDrawPoint (cast gdkWin) (cast gc) x x
	gdkDrawRectangle (cast gdkWin) (cast gc) True 10 10 200 200
--	gtkMainQuit gw ud
