{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable #-}

import Data.Typeable
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import System.Environment
import Foreign.Marshal
import Foreign.Storable
import Control.Applicative

import Hierarchy
import GtkFunHs

main :: IO ()
main = do
	args <- getArgs
	gtkInit args
	win <- gtkWindowNew
	button <- gtkButtonNewWithLabel "Button"
	case (castGObject win, castGObject win, castGObject button) of
		(Just w, Just wc, Just b) -> do
			gtkContainerAdd wc b
			gSignalConnectData b "clicked" gtkMainQuit nullPtr nullPtr 0
			gSignalConnectData w "destroy" gtkMainQuit nullPtr nullPtr 0
			gtkWidgetShow w
			gtkMain
		_ -> error "bad"
