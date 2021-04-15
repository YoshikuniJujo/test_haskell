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

class PangoFontDescriptionAxis a where
	pangoFontDescriptionAxisTag :: BS.ByteString
	pangoFontDescriptionAxisToDouble :: a -> Double
	pangoFontDescriptionAxisFromDouble :: Double -> a

pangoFontDescriptionSetAxis :: forall a m .
	(PangoFontDescriptionAxis a, PrimMonad m) =>
	PangoFontDescription (PrimState m) -> a -> m ()
pangoFontDescriptionSetAxis fd a = do
	as <- pangoFontDescriptionGetVariationsMap fd
	pangoFontDescriptionSetVariationsMap fd $ M.insert
		(pangoFontDescriptionAxisTag @a)
		(pangoFontDescriptionAxisToDouble a) as

pangoFontDescriptionGetAxis :: forall a m .
	(PangoFontDescriptionAxis a, PrimMonad m) =>
	PangoFontDescription (PrimState m) -> m (Maybe a)
pangoFontDescriptionGetAxis fd = do
	as <- pangoFontDescriptionGetVariationsMap fd
	pure $ pangoFontDescriptionAxisFromDouble
		<$> M.lookup (pangoFontDescriptionAxisTag @a) as

newtype Weight = Weight { getWeight :: Double } deriving Show

instance PangoFontDescriptionAxis Weight where
	pangoFontDescriptionAxisTag = "wght"
	pangoFontDescriptionAxisToDouble = getWeight
	pangoFontDescriptionAxisFromDouble = Weight

type Variations = M.Map BS.ByteString Double

showVariations :: Variations -> BS.ByteString
showVariations = BS.intercalate "," . ((\(a, v) -> a <> "=" <> v) . (id *** BSC.pack . show) <$>) . M.toList

readVariations :: BS.ByteString -> Variations
readVariations = M.fromList . ((\[a, v] -> (a, read $ BSC.unpack v)) . BSC.split '=' <$>) . BSC.split ','

pangoFontDescriptionSetVariationsMap :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> Variations -> m ()
pangoFontDescriptionSetVariationsMap (PangoFontDescription ffd) v = unsafeIOToPrim
	$ withForeignPtr ffd \pfd -> BS.useAsCString (showVariations v) \cv ->
		c_pango_font_description_set_variations pfd cv

pangoFontDescriptionSetVariation :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> String -> m ()
pangoFontDescriptionSetVariation (PangoFontDescription fpfd) f = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd -> withCString f \cf ->
		c_pango_font_description_set_variations pfd cf

foreign import ccall "pango_font_description_set_variations"
	c_pango_font_description_set_variations ::
	Ptr (PangoFontDescription s) -> CString -> IO ()

pangoFontDescriptionGetVariationsMap :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> m Variations
pangoFontDescriptionGetVariationsMap (PangoFontDescription ffd) = unsafeIOToPrim
	$ withForeignPtr ffd \pfd -> readVariations <$>
		(myPackCString =<< c_pango_font_description_get_variations pfd)

myPackCString :: CString -> IO BS.ByteString
myPackCString cs | cs == nullPtr = pure "" | otherwise = BS.packCString cs

foreign import ccall "pango_font_description_get_variations"
	c_pango_font_description_get_variations ::
	Ptr (PangoFontDescription s) -> IO CString
