{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.GpuFragmentObject where

import GHC.JS.Value.GpuShaderModule qualified as JS.GpuShaderModule
import GHC.JS.Value.GpuOverridableConstant
	qualified as JS.GpuOverridableConstant

import GHC.JS.Value.GpuBlendComponent qualified as JS.GpuBlendComponent

data G = G {
	constants :: Maybe JS.GpuOverridableConstant.G,
	entryPoint :: Maybe String,
	gModule :: JS.GpuShaderModule.G
	}
