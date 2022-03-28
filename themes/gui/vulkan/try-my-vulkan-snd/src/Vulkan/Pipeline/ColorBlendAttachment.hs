{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.ColorBlendAttachment where

import Vulkan.Enum
import Vulkan.Base

import qualified Vulkan.ColorComponent.Enum as ColorComponent
import qualified Vulkan.Pipeline.ColorBlendAttachment.Core as C

data State = State {
	stateBlendEnable :: Bool,
	stateSrcColorBlendFactor :: BlendFactor,
	stateDstColorBlendFactor :: BlendFactor,
	stateColorBlendOp :: BlendOp,
	stateSrcAlphaBlendFactor :: BlendFactor,
	stateDstAlphaBlendFactor :: BlendFactor,
	stateAlphaBlendOp :: BlendOp,
	stateColorWriteMask :: ColorComponent.Flags }
	deriving Show

stateToCore :: State -> C.State
stateToCore State {
	stateBlendEnable = boolToBool32 -> be,
	stateSrcColorBlendFactor = BlendFactor scbf,
	stateDstColorBlendFactor = BlendFactor dcbf,
	stateColorBlendOp = BlendOp cbo,
	stateSrcAlphaBlendFactor = BlendFactor sabf,
	stateDstAlphaBlendFactor = BlendFactor dabf,
	stateAlphaBlendOp = BlendOp abo,
	stateColorWriteMask = ColorComponent.FlagBits cwm
	} = C.State {
		C.stateBlendEnable = be,
		C.stateSrcColorBlendFactor = scbf,
		C.stateDstColorBlendFactor = dcbf,
		C.stateColorBlendOp = cbo,
		C.stateSrcAlphaBlendFactor = sabf,
		C.stateDstAlphaBlendFactor = dabf,
		C.stateAlphaBlendOp = abo,
		C.stateColorWriteMask = cwm }
