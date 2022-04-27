{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import GHC.Generics
import Foreign.Concurrent
import Foreign.Storable.SizeAlignment
import Control.Monad.Cont

import Cglm
import Vulkan.Pipeline.VertexInputState.BindingStrideList

import qualified Vulkan.Pipeline.VertexInputState as Vk.Ppl.VertexInputSt
import qualified Vulkan.Pipeline.VertexInputState.Middle as Vk.Ppl.VertexInputSt.M
import qualified Vulkan.Pipeline.VertexInputState.Core as Vk.Ppl.VertexInputSt.C
import qualified Vulkan.VertexInput as Vk.VertexInput

data Vertex = Vertex {
	vertexPos :: Vec2,
	vertexColor :: Vec3 }
	deriving (Show, Generic)

instance SizeAlignmentList Vertex

main :: IO ()
main = do
	let	vertexInputInfo ::
			Vk.Ppl.VertexInputSt.CreateInfo () (
				(AddType [Vertex] 'Vk.VertexInput.RateVertex),
				(AddType [Vertex] 'Vk.VertexInput.RateVertex)) '[]
		vertexInputInfo = Vk.Ppl.VertexInputSt.CreateInfo {
			Vk.Ppl.VertexInputSt.createInfoNext = Nothing,
			Vk.Ppl.VertexInputSt.createInfoFlags =
				Vk.Ppl.VertexInputSt.M.CreateFlagsZero }
	print vertexInputInfo
	($ pure) $ runContT do
		pcore <- Vk.Ppl.VertexInputSt.createInfoToCore vertexInputInfo
		fcore <- lift $ newForeignPtr pcore (pure ())
		let	core = Vk.Ppl.VertexInputSt.C.CreateInfo_ fcore
		lift $ print core
