{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Text.Foreign.MiscYj (

	-- * CONVERSION WITH CSTRING

	-- ** From CString

	cStringToText,

	-- ** To CString

	textToCString, textListToCStringArray

	) where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Control.Monad.Cont.MiscYj

import qualified Data.Text as Txt
import qualified Data.Text.Foreign as Txt


textListToCStringArray :: [Txt.Text] -> (Ptr CString -> IO a) -> IO a
textListToCStringArray txts f =
	(textToCString `mapContM` txts) \cstrl ->
	allocaArray (length txts) \pcstra ->
	pokeArray pcstra cstrl >> f pcstra

textToCString :: Txt.Text -> (CString -> IO a) -> IO a
textToCString t f = Txt.withCStringLen t \(cs, ln) ->
	allocaArray (ln + 1) \cs' -> do
		copyBytes cs' cs ln
		poke (cs' `plusPtr` ln :: Ptr CChar) 0
		f cs'

cStringToText :: CString -> IO Txt.Text
cStringToText cs = Txt.peekCStringLen =<< cstringToCStringLen cs

cstringLength :: CString -> IO Int
cstringLength pc = do
	c <- peek pc
	case c of
		0 -> pure 0
		_ -> (+ 1) <$> cstringLength (pc `plusPtr` 1)

cstringToCStringLen :: CString -> IO CStringLen
cstringToCStringLen cs = (cs ,) <$> cstringLength cs
