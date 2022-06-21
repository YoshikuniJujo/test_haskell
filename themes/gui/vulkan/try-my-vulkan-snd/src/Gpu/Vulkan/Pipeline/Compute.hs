{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications, RankNTypes #-}
{-# LANGUAGE MonoLocalBinds, GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.Compute where

import Foreign.Pointable
import Control.Exception
import Data.HeteroList
import Data.Kind
import Data.Int

import Shaderc.EnumAuto

import Gpu.Vulkan.Pipeline.Enum

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Pipeline.ShaderStage as ShaderStage
import qualified Gpu.Vulkan.Pipeline.Layout.Type as Layout
import qualified Gpu.Vulkan.Pipeline.Cache.Middle as Cache
import qualified Gpu.Vulkan.Pipeline.Compute.Middle as M

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
	Device.D ds -> CreateInfo n n1 n2 c d vs sl sbph ->
	IO (M.CreateInfo n n1 vs)
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

data CreateInfo_ n n1 n2 c d (vsslsbph :: (Type, Type, Type)) where
	CreateInfo_ :: CreateInfo n n1 n2 c d vs sl sbph ->
		CreateInfo_ n n1 n2 c d '(vs, sl, sbph)

type family FirstList (abcs :: [(Type, Type, Type)]) where
	FirstList '[] = '[]
	FirstList ('(a, b, c) ': abcs) = a ': FirstList abcs

createInfoListToMiddle ::
	(HeteroVarListMapM vss (FirstList vss), Pointable n2, Pointable c) =>
	Device.D ds -> HeteroVarList (CreateInfo_ n n1 n2 c d) vss ->
	IO (HeteroVarList (M.CreateInfo n n1) (FirstList vss))
createInfoListToMiddle dvc cis =
	heteroVarListMapM' (createInfoToMiddle dvc . (\(CreateInfo_ ci) -> ci)) cis

destroyCreateInfoMiddle :: Pointable d =>
	Device.D sd ->
	M.CreateInfo n n1 vs -> CreateInfo n n1 n2 c d vs sl sbph -> IO ()
destroyCreateInfoMiddle dvc mci ci = ShaderStage.destroyCreateInfoMiddle dvc
	(M.createInfoStage mci) (createInfoStage ci)

class DestroyCreateInfoMiddleList vss vss' where
	destroyCreateInfoMiddleList ::
		Pointable d =>
		Device.D sd ->
		HeteroVarList (M.CreateInfo n n1) vss ->
		HeteroVarList (CreateInfo_ n n1 n2 c d) vss' -> IO ()

instance DestroyCreateInfoMiddleList '[] '[] where
	destroyCreateInfoMiddleList _ HVNil HVNil = pure ()

instance DestroyCreateInfoMiddleList vss vss' =>
	DestroyCreateInfoMiddleList (vs ': vss) ('(vs, sl, sbph) ': vss') where
	destroyCreateInfoMiddleList
		dvc (mci :...: mcis) (CreateInfo_ ci :...: cis) = do
		destroyCreateInfoMiddle dvc mci ci
		destroyCreateInfoMiddleList dvc mcis cis

createCs :: (
	DestroyCreateInfoMiddleList (FirstList vss) vss,
	M.CreateInfosToCore (FirstList vss),
	HeteroVarListMapM vss (FirstList vss), Pointable n, Pointable n1, Pointable n2,
	Pointable c, Pointable d, Pointable c', Pointable d' ) =>
	Device.D sd -> Maybe Cache.C -> HeteroVarList (CreateInfo_ n n1 n2 c d) vss ->
	Maybe (AllocationCallbacks.A c') -> Maybe (AllocationCallbacks.A d') ->
	(forall s . [C s] -> IO a) -> IO a
createCs dvc@(Device.D mdvc) cch cis macc macd f = do
	cis' <- createInfoListToMiddle dvc cis
	bracket
		(M.createCs mdvc cch cis' macc
			<* destroyCreateInfoMiddleList dvc cis' cis)
		(mapM_ \c -> M.destroy mdvc c macd)
		(f . (C <$>))
