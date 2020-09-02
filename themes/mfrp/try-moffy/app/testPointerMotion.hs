import Graphics.Gtk
import Graphics.Gtk.Cairo

main :: IO ()
main = do
	gtkInit []
	w <- gtkWindowNew gtkWindowToplevel
	gtkWidgetSetEvents w [gdkPointerMotionMask]
	gSignalConnect w Destroy gtkMainQuit ()
	gSignalConnect w MotionNotifyEvent (\a b c -> True <$ print (a, b, c)) ()

	da <- gtkDrawingAreaNew
	gSignalConnect da DrawEvent draw ()
	gtkContainerAdd (castWidgetToContainer w) da

	gtkWidgetShowAll w
	gtkMain

motionNotifyEvent :: GtkWidget -> GdkEventMotion -> a -> IO Bool
motionNotifyEvent w cr _ = True <$ do
	print (w, cr)
	gtkWidgetQueueDraw w

draw :: GtkWidget -> CairoT -> a -> IO Bool
draw w cr _ = True <$ do
	cairoMoveTo cr 100 100
	cairoLineTo cr 500 400
	cairoStroke cr
