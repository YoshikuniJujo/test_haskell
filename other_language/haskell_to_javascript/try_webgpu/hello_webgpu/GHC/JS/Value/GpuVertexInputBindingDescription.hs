{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.GpuVertexInputBindingDescription where

import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.GpuVertexInputAttributeDescription qualified as
	JS.GpuVertexInputAttributeDescription

data G = G {
	arrayStride :: Int,
	attributes :: [JS.GpuVertexInputAttributeDescription.G],
	stepMode :: StepMode
	}

data StepMode = Instance | Vertex deriving Show

instance JS.Value.IsJSVal StepMode where
	toJSVal = \case
		Instance -> JS.Value.toJSVal "instance"
		Vertex -> JS.Value.toJSVal "vertex"
