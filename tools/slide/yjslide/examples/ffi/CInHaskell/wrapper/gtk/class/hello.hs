import Gtk

main :: IO ()
main = do
	gtkInit
	w <- gtkWindowNew gtkWindowToplevel
	gSignalConnect w "destroy" gtkMainQuit
	gtkWidgetShow w
	gtkMain
