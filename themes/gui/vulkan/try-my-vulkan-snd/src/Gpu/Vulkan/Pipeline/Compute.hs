{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications, RankNTypes #-}
{-# LANGUAGE MonoLocalBinds, GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.Compute (
	C(..), CreateInfoOld(..), createCsOld, Pipeline(..)
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
import qualified Gpu.Vulkan.Pipeline.ShaderStage.Internal as ShaderStage
import qualified Gpu.Vulkan.Pipeline.Layout.Type as Layout
import qualified Gpu.Vulkan.Pipeline.Cache.Middle as Cache
import qualified Gpu.Vulkan.Pipeline.Compute.Middle as M

newtype C s = C M.C deriving Show

data CreateInfoOld n nncdvs slsbtss sbph = CreateInfoOld {
	createInfoNextOld :: Maybe n,
	createInfoFlagsOld :: CreateFlags,
	createInfoStageOld :: V6 ShaderStage.CreateInfo nncdvs,
	createInfoLayoutOld :: V3 Layout.L slsbtss,
	createInfoBasePipelineHandleOld :: Maybe (C sbph),
	createInfoBasePipelineIndexOld :: Maybe Int32 }

createInfoToMiddleOld :: (Pointable n', Pointable c) =>
	Device.D ds ->
	CreateInfoOld n '(n1, n', 'GlslComputeShader, c, d, vs) slsbtss sbph ->
	IO (M.CreateInfo n n1 vs)
createInfoToMiddleOld dvc CreateInfoOld {
	createInfoNextOld = mnxt,
	createInfoFlagsOld = flgs,
	createInfoStageOld = stg,
	createInfoLayoutOld = V3 (Layout.L lyt),
	createInfoBasePipelineHandleOld = ((\(C b) -> b) <$>) -> bph,
	createInfoBasePipelineIndexOld = bpi
	} = do
	stg' <- ShaderStage.createInfoToMiddle' dvc stg
	pure M.CreateInfo {
		M.createInfoNext = mnxt,
		M.createInfoFlags = flgs,
		M.createInfoStage = stg',
		M.createInfoLayout = lyt,
		M.createInfoBasePipelineHandle = bph,
		M.createInfoBasePipelineIndex = bpi }

class CreateInfoListToMiddleOld as where
	type ResultOld as :: [(Type, Type, Type)]
	createInfoListToMiddleOld ::
		Device.D sd -> HeteroVarList (V4 CreateInfoOld) as ->
		IO (HeteroVarList (V3 M.CreateInfo) (ResultOld as))

instance CreateInfoListToMiddleOld '[] where
	type ResultOld '[] = '[]
	createInfoListToMiddleOld _ HVNil = pure HVNil

instance (Pointable n', Pointable c, CreateInfoListToMiddleOld as) =>
	CreateInfoListToMiddleOld (
		'(n, '(n1, n', 'GlslComputeShader, c, d, vs), slsbtss, sbph
		) ': as) where
	type ResultOld (
		'(n, '(n1, n', 'GlslComputeShader, c, d, vs), slsbtss, sbph) ':
		as ) = '(n, n1, vs) ': ResultOld as
	createInfoListToMiddleOld dvc (V4 ci :...: cis) = (:...:)
		<$> (V3 <$> createInfoToMiddleOld dvc ci)
		<*> createInfoListToMiddleOld dvc cis

destroyCreateInfoMiddleOld :: Pointable d =>
	Device.D sd ->
	M.CreateInfo n n1 vs -> CreateInfoOld n '(n1, n2, 'GlslComputeShader, c, d, vs) slsbtss sbph -> IO ()
destroyCreateInfoMiddleOld dvc mci ci = ShaderStage.destroyCreateInfoMiddle dvc
	(M.createInfoStage mci) ((\(V6 s) -> s) $ createInfoStageOld ci)

class DestroyCreateInfoMiddleListOld vss vss' where
	destroyCreateInfoMiddleListOld ::
		Device.D sd ->
		HeteroVarList (V3 M.CreateInfo) vss ->
		HeteroVarList (V4 CreateInfoOld) vss' -> IO ()

instance DestroyCreateInfoMiddleListOld '[] '[] where
	destroyCreateInfoMiddleListOld _ HVNil HVNil = pure ()

instance (Pointable d, DestroyCreateInfoMiddleListOld vss vss') =>
	DestroyCreateInfoMiddleListOld
		('(n, n1, vs) ': vss)
		('(n, '(n1, n2, 'GlslComputeShader, c, d, vs), slsbtss, sbph) ': vss') where
	destroyCreateInfoMiddleListOld dvc
		(V3 mci :...: mcis) (V4 ci :...: cis) = do
		destroyCreateInfoMiddleOld dvc mci ci
		destroyCreateInfoMiddleListOld dvc mcis cis

createCsOld :: (
	CreateInfoListToMiddleOld vss, M.CreateInfoListToCore (ResultOld vss),
	Pointable c', Pointable d',
	DestroyCreateInfoMiddleListOld (ResultOld vss) vss,
	PipelineListToHetero (ToDummies vss) ) =>
	Device.D sd -> Maybe Cache.C -> HeteroVarList (V4 CreateInfoOld) vss ->
	Maybe (AllocationCallbacks.A c') -> Maybe (AllocationCallbacks.A d') ->
	(forall s . HeteroVarList (Pipeline s) (ToDummies vss) -> IO a) -> IO a
createCsOld dvc@(Device.D mdvc) cch cis macc macd f = do
	cis' <- createInfoListToMiddleOld dvc cis
	bracket
		(M.createCs mdvc cch cis' macc
			<* destroyCreateInfoMiddleListOld dvc cis' cis)
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
