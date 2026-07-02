{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.GpuFragmentObject where

import GHC.JS.Value.GpuShaderModule qualified as JS.GpuShaderModule
import GHC.JS.Value.GpuOverridableConstant
	qualified as JS.GpuOverridableConstant
import GHC.JS.Value.GpuBlendState qualified as JS.GpuBlendState
import GHC.JS.Value.GpuTextureFormat qualified as JS.GpuTextureFormat

import Data.Bits
import Data.Int

data G = G {
	constants :: Maybe JS.GpuOverridableConstant.G,
	entryPoint :: Maybe String,
	gModule :: JS.GpuShaderModule.G
	}
	deriving Show

data Target = Target {
	blend :: JS.GpuBlendState.G,
	format :: JS.GpuTextureFormat.G,
	writeMask :: ColorWrite }
	deriving Show

newtype ColorWrite = ColorWrite Int32

instance Show ColorWrite where
	show = \case
		ColorWriteRed -> "ColorWriteRed"
		ColorWriteGreen -> "ColorWriteGreen"
		ColorWriteBlue -> "ColorWriteBlue"
		ColorWriteAlpha -> "ColorWriteAlpha"
		ColorWriteAll -> "ColorWriteAll"
		ColorWrite cw -> "(ColorWrite " ++ show cw ++ ")"

pattern ColorWriteRed, ColorWriteGreen, ColorWriteBlue,
	ColorWriteAlpha, ColorWriteAll :: ColorWrite
pattern ColorWriteRed = ColorWrite 0x01
pattern ColorWriteGreen = ColorWrite 0x02
pattern ColorWriteBlue = ColorWrite 0x04
pattern ColorWriteAlpha = ColorWrite 0x08
pattern ColorWriteAll = ColorWrite 0x0f

foreign import javascript "(() => { return GPUColorWrite.RED })"
	js_red :: Int32

foreign import javascript "(() => { return GPUColorWrite.GREEN })"
	js_green :: Int32

foreign import javascript "(() => { return GPUColorWrite.BLUE })"
	js_blue :: Int32

foreign import javascript "(() => { return GPUColorWrite.ALPHA })"
	js_alpha :: Int32

foreign import javascript "(() => { return GPUColorWrite.ALL })"
	js_all :: Int32
