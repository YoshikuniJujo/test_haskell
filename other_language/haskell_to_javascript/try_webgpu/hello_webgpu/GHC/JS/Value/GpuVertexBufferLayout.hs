{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.GpuVertexBufferLayout where

import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import GHC.JS.Value.Array qualified as JS.Array
import GHC.JS.Value.GpuVertexBufferAttributeLayout qualified as
	JS.GpuVertexBufferAttributeLayout

data G = G {
	arrayStride :: Int,
	attributes :: [JS.GpuVertexBufferAttributeLayout.G],
	stepMode :: StepMode
	}

toObject :: G -> IO JS.Object.O
toObject g = do
	o <- JS.Object.new
	JS.Object.set o "arrayStrinde" $ arrayStride g
	JS.Object.set o "attributes"
		=<< JS.Array.fromList
		=<< (JS.GpuVertexBufferAttributeLayout.toObject
			`mapM` attributes g)
	JS.Object.set o "stepMode" $ stepMode g
	pure o

data StepMode = StepModeInstance | StepModeVertex deriving Show

instance JS.Value.IsJSVal StepMode where
	toJSVal = \case
		StepModeInstance -> JS.Value.toJSVal "instance"
		StepModeVertex -> JS.Value.toJSVal "vertex"
instance JS.Value.V StepMode
