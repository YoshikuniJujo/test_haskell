{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications, RankNTypes #-}
{-# LANGUAGE MonoLocalBinds, GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.Compute (
	C(..), CreateInfoNew(..), createCsNew, Pipeline(..)
	) where

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

data CreateInfoNew n nncdvs slsbtss sbph = CreateInfoNew {
	createInfoNextNew :: Maybe n,
	createInfoFlagsNew :: CreateFlags,
	createInfoStageNew :: V6 ShaderStage.CreateInfo nncdvs,
	createInfoLayoutNew :: V3 Layout.LLL slsbtss,
	createInfoBasePipelineHandleNew :: Maybe (C sbph),
	createInfoBasePipelineIndexNew :: Maybe Int32 }

createInfoToMiddleNew :: (Pointable n', Pointable c) =>
	Device.D ds ->
	CreateInfoNew n '(n1, n', 'GlslComputeShader, c, d, vs) slsbtss sbph ->
	IO (M.CreateInfo n n1 vs)
createInfoToMiddleNew dvc CreateInfoNew {
	createInfoNextNew = mnxt,
	createInfoFlagsNew = flgs,
	createInfoStageNew = stg,
	createInfoLayoutNew = V3 (Layout.LLL lyt),
	createInfoBasePipelineHandleNew = ((\(C b) -> b) <$>) -> bph,
	createInfoBasePipelineIndexNew = bpi
	} = do
	stg' <- ShaderStage.createInfoToMiddle' dvc stg
	pure M.CreateInfo {
		M.createInfoNext = mnxt,
		M.createInfoFlags = flgs,
		M.createInfoStage = stg',
		M.createInfoLayout = lyt,
		M.createInfoBasePipelineHandle = bph,
		M.createInfoBasePipelineIndex = bpi }

class CreateInfoListToMiddleNew as where
	type ResultNew as :: [(Type, Type, Type)]
	createInfoListToMiddleNew ::
		Device.D sd -> HeteroVarList (V4 CreateInfoNew) as ->
		IO (HeteroVarList (V3 M.CreateInfo) (ResultNew as))

instance CreateInfoListToMiddleNew '[] where
	type ResultNew '[] = '[]
	createInfoListToMiddleNew _ HVNil = pure HVNil

instance (Pointable n', Pointable c, CreateInfoListToMiddleNew as) =>
	CreateInfoListToMiddleNew (
		'(n, '(n1, n', 'GlslComputeShader, c, d, vs), slsbtss, sbph
		) ': as) where
	type ResultNew (
		'(n, '(n1, n', 'GlslComputeShader, c, d, vs), slsbtss, sbph) ':
		as ) = '(n, n1, vs) ': ResultNew as
	createInfoListToMiddleNew dvc (V4 ci :...: cis) = (:...:)
		<$> (V3 <$> createInfoToMiddleNew dvc ci)
		<*> createInfoListToMiddleNew dvc cis

destroyCreateInfoMiddleNew :: Pointable d =>
	Device.D sd ->
	M.CreateInfo n n1 vs -> CreateInfoNew n '(n1, n2, 'GlslComputeShader, c, d, vs) slsbtss sbph -> IO ()
destroyCreateInfoMiddleNew dvc mci ci = ShaderStage.destroyCreateInfoMiddle dvc
	(M.createInfoStage mci) ((\(V6 s) -> s) $ createInfoStageNew ci)

class DestroyCreateInfoMiddleListNew vss vss' where
	destroyCreateInfoMiddleListNew ::
		Device.D sd ->
		HeteroVarList (V3 M.CreateInfo) vss ->
		HeteroVarList (V4 CreateInfoNew) vss' -> IO ()

instance DestroyCreateInfoMiddleListNew '[] '[] where
	destroyCreateInfoMiddleListNew _ HVNil HVNil = pure ()

instance (Pointable d, DestroyCreateInfoMiddleListNew vss vss') =>
	DestroyCreateInfoMiddleListNew
		('(n, n1, vs) ': vss)
		('(n, '(n1, n2, 'GlslComputeShader, c, d, vs), slsbtss, sbph) ': vss') where
	destroyCreateInfoMiddleListNew dvc
		(V3 mci :...: mcis) (V4 ci :...: cis) = do
		destroyCreateInfoMiddleNew dvc mci ci
		destroyCreateInfoMiddleListNew dvc mcis cis

createCsNew :: (
	CreateInfoListToMiddleNew vss, M.CreateInfosToCore' (ResultNew vss),
	Pointable c', Pointable d',
	DestroyCreateInfoMiddleListNew (ResultNew vss) vss,
	PipelineListToHetero (ToDummies vss) ) =>
	Device.D sd -> Maybe Cache.C -> HeteroVarList (V4 CreateInfoNew) vss ->
	Maybe (AllocationCallbacks.A c') -> Maybe (AllocationCallbacks.A d') ->
	(forall s . HeteroVarList (Pipeline s) (ToDummies vss) -> IO a) -> IO a
createCsNew dvc@(Device.D mdvc) cch cis macc macd f = do
	cis' <- createInfoListToMiddleNew dvc cis
	bracket
		(M.createCs' mdvc cch cis' macc
			<* destroyCreateInfoMiddleListNew dvc cis' cis)
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
