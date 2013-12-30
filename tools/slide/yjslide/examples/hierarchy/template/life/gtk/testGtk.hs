{-# LANGUAGE TemplateHaskell, ExistentialQuantification, DeriveDataTypeable #-}

import Gtk

main :: IO ()
main = do
	gtkInit
	win <- gtkWindowNew
	btn <- gtkButtonNewWithLabel "button"
	case (castGObject win, castGObject win, castGObject btn) of
		(Just w, Just cw, Just b) -> do
			gtkContainerAdd cw b
			gtkWidgetShowAll w
			gSignalConnect w "destroy" gtkMainQuit
			gSignalConnect b "clicked" gtkMainQuit
			gtkMain
		_ -> error "bad"
