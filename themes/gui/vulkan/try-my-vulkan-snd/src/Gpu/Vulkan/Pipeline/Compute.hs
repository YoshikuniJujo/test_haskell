{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications, RankNTypes #-}
{-# LANGUAGE MonoLocalBinds, GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.Compute (

	C(..),

	CreateInfo(..), createCs,

	Pipeline(..) ) where

import Foreign.Storable.PeekPoke
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

data CreateInfo n nncdvs slsbtss sbph = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoStage :: V6 ShaderStage.CreateInfoNew nncdvs,
	createInfoLayout :: V3 Layout.L slsbtss,
	createInfoBasePipelineHandle :: Maybe (C sbph),
	createInfoBasePipelineIndex :: Maybe Int32 }

createInfoToMiddle :: (Pokable n', Pokable c) =>
	Device.D ds ->
	CreateInfo n '(n1, n', 'GlslComputeShader, c, d, vs) slsbtss sbph ->
	IO (M.CreateInfo n n1 vs)
createInfoToMiddle dvc CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoStage = stg,
	createInfoLayout = V3 (Layout.L lyt),
	createInfoBasePipelineHandle = ((\(C b) -> b) <$>) -> bph,
	createInfoBasePipelineIndex = bpi
	} = do
	stg' <- ShaderStage.createInfoToMiddleFooNew dvc stg
	pure M.CreateInfo {
		M.createInfoNext = mnxt,
		M.createInfoFlags = flgs,
		M.createInfoStage = stg',
		M.createInfoLayout = lyt,
		M.createInfoBasePipelineHandle = bph,
		M.createInfoBasePipelineIndex = bpi }

class CreateInfoListToMiddle as where
	type Result as :: [(Type, Type, [Type])]
	createInfoListToMiddle ::
		Device.D sd -> HeteroVarList (V4 CreateInfo) as ->
		IO (HeteroVarList (V3 M.CreateInfo) (Result as))

instance CreateInfoListToMiddle '[] where
	type Result '[] = '[]
	createInfoListToMiddle _ HVNil = pure HVNil

instance (Pokable n', Pokable c, CreateInfoListToMiddle as) =>
	CreateInfoListToMiddle (
		'(n, '(n1, n', 'GlslComputeShader, c, d, vs), slsbtss, sbph
		) ': as) where
	type Result (
		'(n, '(n1, n', 'GlslComputeShader, c, d, vs), slsbtss, sbph) ':
		as ) = '(n, n1, vs) ': Result as
	createInfoListToMiddle dvc (V4 ci :...: cis) = (:...:)
		<$> (V3 <$> createInfoToMiddle dvc ci)
		<*> createInfoListToMiddle dvc cis

destroyCreateInfoMiddle :: Pokable d =>
	Device.D sd ->
	M.CreateInfo n n1 vs -> CreateInfo n '(n1, n2, 'GlslComputeShader, c, d, vs) slsbtss sbph -> IO ()
destroyCreateInfoMiddle dvc mci ci = ShaderStage.destroyCreateInfoMiddleNew dvc
	(M.createInfoStage mci) ((\(V6 s) -> s) $ createInfoStage ci)

class DestroyCreateInfoMiddleList vss vss' where
	destroyCreateInfoMiddleList ::
		Device.D sd ->
		HeteroVarList (V3 M.CreateInfo) vss ->
		HeteroVarList (V4 CreateInfo) vss' -> IO ()

instance DestroyCreateInfoMiddleList '[] '[] where
	destroyCreateInfoMiddleList _ HVNil HVNil = pure ()

instance (Pokable d, DestroyCreateInfoMiddleList vss vss') =>
	DestroyCreateInfoMiddleList
		('(n, n1, vs) ': vss)
		('(n, '(n1, n2, 'GlslComputeShader, c, d, vs), slsbtss, sbph) ': vss') where
	destroyCreateInfoMiddleList dvc
		(V3 mci :...: mcis) (V4 ci :...: cis) = do
		destroyCreateInfoMiddle dvc mci ci
		destroyCreateInfoMiddleList dvc mcis cis

createCs :: (
	CreateInfoListToMiddle vss, M.CreateInfoListToCore (Result vss),
	Pokable c', Pokable d',
	DestroyCreateInfoMiddleList (Result vss) vss,
	PipelineListToHetero (ToDummiesNew vss) ) =>
	Device.D sd -> Maybe Cache.C -> HeteroVarList (V4 CreateInfo) vss ->
	Maybe (AllocationCallbacks.A c') -> Maybe (AllocationCallbacks.A d') ->
	(forall s . HeteroVarList (Pipeline s) (ToDummiesNew vss) -> IO a) -> IO a
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

type family ToDummiesNew tl where
	ToDummiesNew '[] = '[]
	ToDummiesNew (t ': ts) = '() ': ToDummiesNew ts

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
