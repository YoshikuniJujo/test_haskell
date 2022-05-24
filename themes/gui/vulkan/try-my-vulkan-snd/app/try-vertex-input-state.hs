{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import GHC.Generics
import GHC.Tuple
import Foreign.Concurrent
import Foreign.Storable.SizeAlignment
import Control.Monad.Cont

import Cglm
import Vulkan.Pipeline.VertexInputState.BindingStrideList

import qualified Vulkan.Pipeline.VertexInputState as Vk.Ppl.VertexInputSt
import qualified Vulkan.Pipeline.VertexInputState.Middle as Vk.Ppl.VertexInputSt.M
import qualified Vulkan.Pipeline.VertexInputState.Core as Vk.Ppl.VertexInputSt.C
import qualified Vulkan.VertexInput as Vk.VertexInput
import qualified Vulkan.Format.Enum as Vk.Format

data Vertex = Vertex {
	vertexPos :: Vec2,
	vertexColor :: Vec3 }
	deriving (Show, Generic)

instance SizeAlignmentList Vertex

instance SizeAlignmentListUntil Cglm.Vec2 Vertex
instance SizeAlignmentListUntil Cglm.Vec3 Vertex

instance Vk.Ppl.VertexInputSt.Formattable Vec2 where
	formatOf = Vk.Format.R32g32Sfloat

instance Vk.Ppl.VertexInputSt.Formattable Vec3 where
	formatOf = Vk.Format.R32g32b32Sfloat

main :: IO ()
main = do
	let	vertexInputInfo ::
			Vk.Ppl.VertexInputSt.CreateInfo ()
				(Solo (AddType
					[Vertex] 'Vk.VertexInput.RateVertex))
				'[Vec2, Vec3]
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
	print $ Vk.Ppl.VertexInputSt.createInfoToMiddle vertexInputInfo
