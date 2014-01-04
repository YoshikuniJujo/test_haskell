#include <gtk/gtk.h>

module GdkEvent (
	GdkEventPtr,
	GdkEvent(..),
	GdkEventKey,
	gdkEventKeyGetKeyval,
) where

import Control.Applicative

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types

import GObject

data GdkEventPtr

class GdkEvent e where
	fromGdkEventPtr :: Ptr GdkEventPtr -> e

data GdkEventKey = GdkEventKey (Ptr GdkEventKey) deriving Show
instance GdkEvent GdkEventKey where
	fromGdkEventPtr = GdkEventKey . castPtr
instance Pointable GdkEventKey where
	toNullPointer (GdkEventKey p) = return $ castPtr p
	fromNullPointer p = return $ GdkEventKey $ castPtr p
	freePointer _ _ = return ()

c_gdkEventKeyGetKeyval :: Ptr GdkEventKey -> IO CInt
c_gdkEventKeyGetKeyval = #peek GdkEventKey, keyval

gdkEventKeyGetKeyval :: GdkEventKey -> IO Int
gdkEventKeyGetKeyval (GdkEventKey p) = fromIntegral <$> c_gdkEventKeyGetKeyval p
