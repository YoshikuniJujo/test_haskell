{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.GpuFragmentObject where

import GHC.JS.Value.GpuShaderModule qualified as JS.GpuShaderModule
import GHC.JS.Value.GpuOverridableConstant
	qualified as JS.GpuOverridableConstant
import GHC.JS.Value.GpuBlendState qualified as JS.GpuBlendState
import GHC.JS.Value.GpuTextureFormat qualified as JS.GpuTextureFormat

import Data.Int

data G = G {
	constants :: Maybe JS.GpuOverridableConstant.G,
	entryPoint :: Maybe String,
	gModule :: JS.GpuShaderModule.G
	}
	deriving Show

data Target = Target {
	blend :: JS.GpuBlendState.G,
	format :: JS.GpuTextureFormat.G
	}
	deriving Show

newtype WriteMask = WiteMask Int32

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
