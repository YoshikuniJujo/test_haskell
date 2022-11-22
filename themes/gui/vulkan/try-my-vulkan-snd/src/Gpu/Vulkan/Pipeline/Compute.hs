{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications, RankNTypes #-}
{-# LANGUAGE MonoLocalBinds, GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
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

import qualified Gpu.Vulkan.DescriptorSetLayout.Type as DescriptorSetLayout

newtype C s = C M.C deriving Show

data CreateInfo n n1 n2 c d vs sl sbtss sbph = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoStage ::
		ShaderStage.CreateInfo n1 n2 'GlslComputeShader c d vs,
	createInfoLayout :: Layout.LL sl sbtss,
	createInfoBasePipelineHandle :: Maybe (C sbph),
	createInfoBasePipelineIndex :: Maybe Int32 }

data CreateInfo' n nncdvs sl sbtss sbph = CreateInfo' {
	createInfoNext' :: Maybe n,
	createInfoFlags' :: CreateFlags,
	createInfoStage' :: V6 ShaderStage.CreateInfo nncdvs,
	createInfoLayout' :: Layout.LL sl sbtss,
	createInfoBasePipelineHandle' :: Maybe (C sbph),
	createInfoBasePipelineIndex' :: Maybe Int32 }

data CreateInfoNew n n1 n2 c d vs slsbtss sbph = CreateInfoNew {
	createInfoNextNew :: Maybe n,
	createInfoFlagsNew :: CreateFlags,
	createInfoStageNew ::
		ShaderStage.CreateInfo n1 n2 'GlslComputeShader c d vs,
	createInfoLayoutNew :: V3 Layout.LLL slsbtss,
	createInfoBasePipelineHandleNew :: Maybe (C sbph),
	createInfoBasePipelineIndexNew :: Maybe Int32 }

createInfoToMiddle :: (Pointable n2, Pointable c) =>
	Device.D ds -> CreateInfo n n1 n2 c d vs sl sbtss sbph ->
	IO (M.CreateInfo n n1 vs)
createInfoToMiddle dvc CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoStage = stg,
	createInfoLayout = Layout.LL lyt,
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

{-
createInfoToMiddle' :: (Pointable n2, Pointable c) =>
	Device.D ds -> CreateInfo' n nncdvs sl sbtss sbph ->
	IO (M.CreateInfo n n1 vs)
createInfoToMiddle' dvc CreateInfo' {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoStage = V6 stg,
	createInfoLayout = Layout.LL lyt,
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
		-}

-- data CreateInfo_ n n1 n2 c d (vsslsbph :: (Type, Type, Type, Type)) where
data CreateInfo_ n n1 n2 c d vsslsbph where
	CreateInfo_ :: CreateInfo n n1 n2 c d vs sl sbtss sbph ->
		CreateInfo_ n n1 n2 c d '(vs, sl, sbtss, sbph)

type family FirstList (abcs :: [(Type, Type, [(Type, [DescriptorSetLayout.BindingType])], Type)]) where
	FirstList '[] = '[]
	FirstList ('(a, b, c, d) ': abcs) = a ': FirstList abcs

createInfoListToMiddle ::
	(HeteroVarListMapM'' [(Type, [DescriptorSetLayout.BindingType])] vss (FirstList vss), Pointable n2, Pointable c) =>
	Device.D ds -> HeteroVarList (CreateInfo_ n n1 n2 c d) vss ->
	IO (HeteroVarList (M.CreateInfo n n1) (FirstList vss))
createInfoListToMiddle dvc cis =
	heteroVarListMapM'' (createInfoToMiddle dvc . (\(CreateInfo_ ci) -> ci)) cis

destroyCreateInfoMiddle :: Pointable d =>
	Device.D sd ->
	M.CreateInfo n n1 vs -> CreateInfo n n1 n2 c d vs sl sbtss sbph -> IO ()
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
	DestroyCreateInfoMiddleList (vs ': vss) ('(vs, sl, sbtss, sbph) ': vss') where
	destroyCreateInfoMiddleList
		dvc (mci :...: mcis) (CreateInfo_ ci :...: cis) = do
		destroyCreateInfoMiddle dvc mci ci
		destroyCreateInfoMiddleList dvc mcis cis

createCs :: (
	PipelineListToHetero (ToDummies vss),
	DestroyCreateInfoMiddleList (FirstList vss) vss,
	M.CreateInfosToCore (FirstList vss),
	HeteroVarListMapM'' [(Type, [DescriptorSetLayout.BindingType])] vss (FirstList vss),
	Pointable n, Pointable n1, Pointable n2,
	Pointable c, Pointable d, Pointable c', Pointable d' ) =>
	Device.D sd -> Maybe Cache.C -> HeteroVarList (CreateInfo_ n n1 n2 c d) vss ->
	Maybe (AllocationCallbacks.A c') -> Maybe (AllocationCallbacks.A d') ->
	(forall s . HeteroVarList (Pipeline s) (ToDummies vss) -> IO a) -> IO a
createCs dvc@(Device.D mdvc) cch cis macc macd f = do
	cis' <- createInfoListToMiddle dvc cis
	bracket
		(M.createCs mdvc cch cis' macc
			<* destroyCreateInfoMiddleList dvc cis' cis)
		(mapM_ \c -> M.destroy mdvc c macd)
		(f . pipelineListToHetero . (C <$>))

type family ToDummies tl where
	ToDummies '[] = '[]
	ToDummies (t ': ts) = '() ': ToDummies ts

newtype Pipeline s (d :: ()) = Pipeline { unPipeline :: C s } deriving Show

class PipelineListToHetero ds where
	pipelineListToHetero :: [C s] -> HeteroVarList (Pipeline s) ds

instance PipelineListToHetero '[] where
	pipelineListToHetero [] = HVNil
	pipelineListToHetero _ = error "mismatch"

instance PipelineListToHetero ds => PipelineListToHetero ('() ': ds) where
	pipelineListToHetero (p : ps) = Pipeline p :...: pipelineListToHetero ps

class HeteroVarListMapM'' k ss fss where
	heteroVarListMapM'' :: Applicative m =>
		(forall a b (c :: k) d . t '(a, b, c, d) -> m (t' a)) ->
		HeteroVarList t ss -> m (HeteroVarList t' fss)

instance HeteroVarListMapM'' k '[] '[] where
	heteroVarListMapM'' _ HVNil = pure HVNil

instance HeteroVarListMapM'' k ss fss =>
	HeteroVarListMapM'' k ('(a, b, c, d) ': ss) (a ': fss) where
	heteroVarListMapM'' f (x :...: xs) =
		(:...:) <$> f x <*> heteroVarListMapM'' f xs
