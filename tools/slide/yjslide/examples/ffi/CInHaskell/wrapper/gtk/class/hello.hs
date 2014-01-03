import Gtk

main :: IO ()
main = do
	gtkInit
	w <- gtkWindowNew gtkWindowToplevel
--	w <- gtkWindowNew gtkWindowPopup
	gSignalConnect w "destroy" gtkMainQuit
	gtkWidgetShow w
	gtkMain
