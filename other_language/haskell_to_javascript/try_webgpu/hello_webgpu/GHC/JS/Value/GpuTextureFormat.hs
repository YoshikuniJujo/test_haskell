{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.GpuTextureFormat where

import GHC.JS.Prim (toJSString)
import GHC.JS.Value qualified as JS.Value

newtype G = G String

instance Show G where
	show = \case
		R8Unorm -> "r8unorm"
		Rgba8Unorm -> "rgba8unorm"
		Bgra8Unorm -> "bgra8unorm"
		G g -> "(G " ++ show g ++ ")"

instance JS.Value.IsJSVal G where toJSVal (G g) = JS.Value.toJSVal g
instance JS.Value.V G

data CanvasConfig
	= CanvasConfigBgra8Unorm
	| CanvasConfigRgba8Unorm
	| CanvasConfigRgba16Float
	deriving Show

instance JS.Value.IsJSVal CanvasConfig where
	toJSVal = JS.Value.toJSVal . fromCanvasConfig
instance JS.Value.V CanvasConfig

data PreferredCanvas
	= PreferredCanvasRgba8Unorm
	| PreferredCanvasBgra8Unorm
	deriving Show

instance JS.Value.IsJSVal PreferredCanvas where
	toJSVal = JS.Value.toJSVal . fromPreferredCanvas
instance JS.Value.V PreferredCanvas

preferredCanvasToConfig :: PreferredCanvas -> CanvasConfig
preferredCanvasToConfig = \case
	PreferredCanvasRgba8Unorm -> CanvasConfigRgba8Unorm
	PreferredCanvasBgra8Unorm -> CanvasConfigBgra8Unorm

fromCanvasConfig :: CanvasConfig -> G
fromCanvasConfig = \case
	CanvasConfigBgra8Unorm -> Bgra8Unorm
	CanvasConfigRgba8Unorm -> Rgba8Unorm
	CanvasConfigRgba16Float -> Rgba16Float

fromPreferredCanvas :: PreferredCanvas -> G
fromPreferredCanvas = \case
	PreferredCanvasRgba8Unorm -> Rgba8Unorm
	PreferredCanvasBgra8Unorm -> Bgra8Unorm

pattern R8Unorm :: G
pattern R8Unorm = G "r8unorm"

pattern Rgba8Unorm :: G
pattern Rgba8Unorm = G "rgba8unorm"

pattern Bgra8Unorm :: G
pattern Bgra8Unorm = G "bgra8unorm"

pattern Rgba16Float :: G
pattern Rgba16Float = G "rgba16float"
