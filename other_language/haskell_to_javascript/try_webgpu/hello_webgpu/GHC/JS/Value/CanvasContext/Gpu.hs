{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.CanvasContext.Gpu where

import Data.Bits
import Data.Int

import GHC.JS.Prim (JSVal)
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import GHC.JS.Value.Array qualified as JS.Array
import GHC.JS.Value.GpuDevice qualified as JS.GpuDevice
import GHC.JS.Value.CanvasContext qualified as JS.CanvasContext

newtype G = G { unG :: JSVal }

instance Show G where
	show g = "(" ++ JS.Object.toString (JS.Object.toO g) ++ ")"

instance JS.Value.IsJSVal G where toJSVal (G v) = v

instance JS.Value.V G where
	toV = JS.CanvasContext.toValue; fromV = JS.CanvasContext.fromValue

instance JS.Object.IsO G

instance JS.CanvasContext.IsC G where
	downCheck x = JS.Object.toO x `JS.Object.isInstanceOf` rClass
	downMake = G

rClass :: JS.Object.Class
rClass = JS.Object.Class js_GpuCanvasContext

foreign import javascript "(() => { return GPUCanvasContext; })"
	js_GpuCanvasContext :: JSVal

configure :: G -> Configuration -> IO ()
configure g c = configure' g =<< configurationToObject c

configure' :: G -> JS.Object.O -> IO ()
configure' (G g) (JS.Value.toJSVal -> as) = js_configure g as

foreign import javascript "((g, as) => { g.configure(as); })"
	js_configure :: JSVal -> JSVal -> IO ()

configurationToObject :: Configuration -> IO JS.Object.O
configurationToObject c = do
	o <- JS.Object.new
	maybe (pure ()) (JS.Object.set o "alphaMode" . toString) $ alphaMode c
	maybe (pure ()) (JS.Object.set o "colorSpace" . toString) $ colorSpace c
	JS.Object.set o "device" $ device c
	JS.Object.set o "format" . toString $ format c
	maybe (pure ()) (JS.Object.set o "usage" . toInt) $ usage c
	marr <- maybe (pure Nothing)
		((Just <$>) . JS.Array.fromList) $ viewFormats c
	maybe (pure ()) (JS.Object.set o "viewFormats") marr
	pure o

data Configuration = Configuration {
	alphaMode :: Maybe AlphaMode,
	colorSpace :: Maybe ColorSpace,
	device :: JS.GpuDevice.G,
	format :: Format,
	usage :: Maybe GpuTextureUsage,
	viewFormats :: Maybe [String] }
	deriving Show

configuration :: JS.GpuDevice.G -> Format -> Configuration
configuration dvc fmt = Configuration {
	alphaMode = Nothing, colorSpace = Nothing,
	device = dvc, format = fmt,
	usage = Nothing, viewFormats = Nothing }

data AlphaMode = AlphaModeOpaque | AlphaModePremultiplied deriving Show
data ColorSpace = ColorSpaceSrgb | ColorSpaceDisplayP3 deriving Show
data Format
	= FormatBgra8Unorm | FormatRgba8Unorm | FormatRgba16Float deriving Show
newtype GpuTextureUsage = GpuTextureUsage Int32 deriving (Eq, Bits)

class ToString a where toString :: a -> String
class FromString a where fromString :: String -> Maybe a

instance ToString AlphaMode where
	toString = \case
		AlphaModeOpaque -> "opaque"
		AlphaModePremultiplied -> "premultiplied"

instance ToString ColorSpace where
	toString = \case
		ColorSpaceSrgb -> "srgb"
		ColorSpaceDisplayP3 -> "display-p3"

instance ToString Format where
	toString = \case
		FormatBgra8Unorm -> "bgra8unorm"
		FormatRgba8Unorm -> "rgba8unorm"
		FormatRgba16Float -> "rgba16float"

instance FromString Format where
	fromString = \case
		"bgra8unorm" -> Just FormatBgra8Unorm
		"rgba8unorm" -> Just FormatRgba8Unorm
		"rgba16float" -> Just FormatRgba16Float
		_ -> Nothing

class ToInt a where toInt :: a -> Int

instance ToInt GpuTextureUsage where toInt (GpuTextureUsage n) = fromIntegral n

instance Show GpuTextureUsage where
	show = \case
		GpuTextureUsageCopySrc -> "GpuTextureUsageCopySrc"
		GpuTextureUsageCopyDst -> "GpuTextureUsageCopyDst"
		GpuTextureUsageRenderAttachment ->
			"GpuTextureUsageRenderAttachment"
		GpuTextureUsageStorageBinding -> "GpuTextureUsageStorageBinding"
		GpuTextureUsageTextureBinding -> "GpuTextureUsageTextureBinding"
		GpuTextureUsageTransientAttachment ->
			"GpuTextureUsageTransientAttachment"
		GpuTextureUsage n -> "(GpuTextureUsage " ++ show n ++ ")"

pattern GpuTextureUsageCopySrc, GpuTextureUsageCopyDst,
	GpuTextureUsageRenderAttachment,
	GpuTextureUsageTextureBinding, GpuTextureUsageStorageBinding,
	GpuTextureUsageTransientAttachment
	:: GpuTextureUsage
pattern GpuTextureUsageCopySrc = GpuTextureUsage 0x01
pattern GpuTextureUsageCopyDst = GpuTextureUsage 0x02
pattern GpuTextureUsageRenderAttachment = GpuTextureUsage 0x10
pattern GpuTextureUsageStorageBinding = GpuTextureUsage 0x08
pattern GpuTextureUsageTextureBinding = GpuTextureUsage 0x04
pattern GpuTextureUsageTransientAttachment = GpuTextureUsage 0x20
