import Data.Maybe
import Foreign.Ptr
import Foreign.Marshal
import Gtk

main :: IO ()
main = do
	gtkInit
	w <- gtkWindowNew
	b <- gtkButtonNewWithLabel "button"
	gSignalConnect (cast w) "destroy" gtkMainQuit
	gSignalConnectData (cast b) "clicked" buttonClicked "hello" zero
	gtkContainerAdd (cast w) (cast b)
	gtkWidgetShowAll (cast w)
	gtkMain

cast :: (GObject g1, GObject g2) => g1 -> g2
cast = fromJust . castGObject

buttonClicked :: GtkWidget -> String -> IO ()
buttonClicked w s = do
	print $ pointer w
	print s

destroyData :: Ptr () -> GClosure -> IO ()
destroyData m c = do
	putStrLn $ "destroy: " ++ show m
	putStrLn $ "closure: " ++ show c
	free m
