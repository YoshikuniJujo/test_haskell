{-# LANGUAGE BlockArguments, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Text.Tools (pokeText, cstringToText) where

import Foreign.Ptr
import Foreign.Marshal.Utils
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String

import qualified Data.Text as Txt
import qualified Data.Text.Foreign as Txt

pokeText :: Int -> CString -> Txt.Text -> IO ()
pokeText mx dst t = Txt.withCStringLen t \(src, ln) -> do
	let	ln' = min ln (mx - 1)
	copyBytes dst src ln'
	poke (dst `plusPtr` ln' :: Ptr CChar) 0

cstringToText :: CString -> IO Txt.Text
cstringToText cs = Txt.peekCStringLen =<< cstringToCStringLen cs

cstringToCStringLen :: CString -> IO CStringLen
cstringToCStringLen cs = (cs ,) <$> cstringLength cs

cstringLength :: CString -> IO Int
cstringLength pc = do
	c <- peek pc
	case c of
		0 -> pure 0
		_ -> (+ 1) <$> cstringLength (pc `plusPtr` 1)
