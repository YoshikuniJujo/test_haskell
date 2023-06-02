{-# LANGUAGE BlockArguments, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Text.Foreign.Misc where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String

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

cstrToText :: CString -> IO Txt.Text
cstrToText cs = Txt.peekCStringLen =<< cstringToCStringLen cs

cstringLength :: CString -> IO Int
cstringLength pc = do
	c <- peek pc
	case c of
		0 -> pure 0
		_ -> (+ 1) <$> cstringLength (pc `plusPtr` 1)

cstringToCStringLen :: CString -> IO CStringLen
cstringToCStringLen cs = (cs ,) <$> cstringLength cs

mapContM :: Monad m => (a -> (b -> m c) -> m c) -> [a] -> ([b] -> m c) -> m c
-- mapContM f = runContT . mapM (ContT . f)
mapContM _ [] g = g []
mapContM f (x : xs) g = f x \y -> mapContM f xs \ys -> g $ y : ys
