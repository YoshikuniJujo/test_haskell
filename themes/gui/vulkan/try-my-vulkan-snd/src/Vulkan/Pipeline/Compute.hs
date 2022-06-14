{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.Compute where

import Foreign.Pointable
import Data.Int

import Shaderc.EnumAuto

import Vulkan.Pipeline.Enum

import qualified Vulkan.Device as Device
import qualified Vulkan.Pipeline.ShaderStage as ShaderStage
import qualified Vulkan.Pipeline.Layout.Type as Layout
import qualified Vulkan.Pipeline.Compute.Middle as M

newtype C s = C M.C deriving Show

data CreateInfo n n1 n2 c d vs sl sbph = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoStage ::
		ShaderStage.CreateInfo n1 n2 'GlslComputeShader c d vs,
	createInfoLayout :: Layout.L sl,
	createInfoBasePipelineHandle :: Maybe (C sbph),
	createInfoBasePipelineIndex :: Maybe Int32 }

createInfoToMiddle :: (Pointable n2, Pointable c) =>
	Device.D ds -> CreateInfo n n1 n2 c d vs sl sbph -> IO (M.CreateInfo n n1 vs)
createInfoToMiddle dvc CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoStage = stg,
	createInfoLayout = Layout.L lyt,
	createInfoBasePipelineHandle = ((\(C b) -> b) <$>) -> bph,
	createInfoBasePipelineIndex = bpi
	} = do
	stg' <- ShaderStage.createInfoToMiddle dvc stg
	pure M.CreateInfo {
		M.createInfoNext = mnxt,
		M.createInfoFlags = flgs,
		M.createInfoStage = stg',
		M.createInfoLayout = lyt,
		M.createInfoBasePipelineHandle = bph,
		M.createInfoBasePipelineIndex = bpi }

destroyCreateInfoMiddle :: Pointable d =>
	Device.D ds ->
	M.CreateInfo n n1 vs -> CreateInfo n n1 n2 c d vs sl sbph -> IO ()
destroyCreateInfoMiddle dvc mci ci = ShaderStage.destroyCreateInfoMiddle dvc
	(M.createInfoStage mci) (createInfoStage ci)
