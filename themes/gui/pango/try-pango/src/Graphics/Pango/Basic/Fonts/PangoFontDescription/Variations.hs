{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.Fonts.PangoFontDescription.Variations where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.String
import Control.Arrow
import Control.Monad.Primitive

import Graphics.Pango.Basic.Fonts.PangoFontDescription.Type

import qualified Data.Map as M
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

type Variations = M.Map BS.ByteString Double

showVariations :: Variations -> BS.ByteString
showVariations = BS.intercalate "," . ((\(a, v) -> a <> "=" <> v) . (id *** BSC.pack . show) <$>) . M.toList

readVariations :: BS.ByteString -> Variations
readVariations = M.fromList . ((\[a, v] -> (a, read $ BSC.unpack v)) . BSC.split '=' <$>) . BSC.split ','

pangoFontDescriptionSetVariation :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> String -> m ()
pangoFontDescriptionSetVariation (PangoFontDescription fpfd) f = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd -> withCString f \cf ->
		c_pango_font_description_set_variations pfd cf

foreign import ccall "pango_font_description_set_variations"
	c_pango_font_description_set_variations ::
	Ptr (PangoFontDescription s) -> CString -> IO ()
