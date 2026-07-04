{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.GpuVertexBufferLayout where

import System.IO.Unsafe

import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import GHC.JS.Value.Array qualified as JS.Array
import GHC.JS.Value.GpuVertexBufferAttributeLayout qualified as
	JS.GpuVertexBufferAttributeLayout

data G = G {
	arrayStride :: Int,
	attributes :: [JS.GpuVertexBufferAttributeLayout.G],
	stepMode :: StepMode }
	deriving Show

toObject :: G -> IO JS.Object.O
toObject g = do
	o <- JS.Object.new @JS.Object.IO
	JS.Object.set o "arrayStrinde" $ arrayStride g
	JS.Object.set o "attributes"
		=<< JS.Array.fromList
		=<< (JS.GpuVertexBufferAttributeLayout.toObject
			`mapM` attributes g)
	JS.Object.set o "stepMode" $ stepMode g
	JS.Object.freeze o

instance JS.Value.IsJSVal G where
	toJSVal = unsafePerformIO . (JS.Value.toJSVal <$>) . toObject

instance JS.Value.V G where toV = JS.Object.toValue; fromV = JS.Object.fromValue

data StepMode = StepModeInstance | StepModeVertex deriving Show

instance JS.Value.IsJSVal StepMode where
	toJSVal = \case
		StepModeInstance -> JS.Value.toJSVal "instance"
		StepModeVertex -> JS.Value.toJSVal "vertex"
instance JS.Value.V StepMode
