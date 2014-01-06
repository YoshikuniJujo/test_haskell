#include <gtk/gtk.h>

module GdkEvent (
	GdkEventPtr,
	GdkEvent(..),
	GdkEventKey,
	Keyval,
	char2keyval,
	gdkEventKeyGetKeyval,
	gdkEventKeyGetString,
) where

import Control.Applicative
import Data.Char

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String

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

data Keyval = Keyval Int deriving (Show, Eq)
char2keyval :: Char -> Keyval
char2keyval ' ' = Keyval 32
char2keyval ';' = Keyval 59
char2keyval '.' = Keyval 46
char2keyval '/' = Keyval 47
char2keyval c
	| isLower c = Keyval $ ord c
	| otherwise = error "bad key"
c_gdkEventKeyGetKeyval :: Ptr GdkEventKey -> IO CInt
c_gdkEventKeyGetKeyval = #peek GdkEventKey, keyval
gdkEventKeyGetKeyval :: GdkEventKey -> IO Keyval
gdkEventKeyGetKeyval (GdkEventKey p) = Keyval . fromIntegral <$> c_gdkEventKeyGetKeyval p

c_gdkEventKeyGetString :: Ptr GdkEventKey -> IO CString
c_gdkEventKeyGetString = #peek GdkEventKey, string
gdkEventKeyGetString :: GdkEventKey -> IO String
gdkEventKeyGetString (GdkEventKey p) = peekCString =<< c_gdkEventKeyGetString p
