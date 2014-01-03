import Gtk
import Data.Maybe

main :: IO ()
main = do
	gtkInit
	w <- gtkWindowNew
	gtkWidgetShow (cast w)
	gtkMain

cast :: (GObject g1, GObject g2) => g1 -> g2
cast = fromJust . castGObject
