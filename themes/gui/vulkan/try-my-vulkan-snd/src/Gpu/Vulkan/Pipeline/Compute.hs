{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications, RankNTypes #-}
{-# LANGUAGE MonoLocalBinds, GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.Compute (
	C(..), CNew(..), CreateInfo(..), createCs, createCsNew ) where

import Foreign.Storable.PeekPoke
import Control.Exception
import Data.TypeLevel.Uncurry
import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:**))
import Data.Kind
import Data.Int

import Shaderc.EnumAuto

import Gpu.Vulkan.Pipeline.Enum

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Pipeline.ShaderStage.Internal as ShaderStage
import qualified Gpu.Vulkan.DescriptorSetLayout.Type as DescriptorSetLayout
import qualified Gpu.Vulkan.Pipeline.Layout.Type as Layout
import qualified Gpu.Vulkan.PipelineCache.Middle as Cache
import qualified Gpu.Vulkan.Pipeline.Compute.Middle as M

import Gpu.Vulkan.DescriptorSetLayout.Type qualified as DscStLyt

newtype C s = C M.C deriving Show

newtype CNew s (slbtss :: (Type, [(Type, [DescriptorSetLayout.BindingType])], [Type])) = CNew M.C deriving Show

data CreateInfo n nncdvs slsbtss sbph = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoStage :: U6 ShaderStage.CreateInfoNew nncdvs,
	createInfoLayout :: U3 Layout.L slsbtss,
	createInfoBasePipelineHandleOrIndex ::
		Maybe (Either (U2 CNew sbph) Int32) }

type CreateInfoArgs4 = (
	Type,
	(Type, Type, Shaderc.EnumAuto.ShaderKind, Type, Type, [Type]),
	(Type, [(Type, [DscStLyt.BindingType])], [Type]),
	(Type, (Type, [(Type, [DscStLyt.BindingType])], [Type])) )

type CArgs1 = (Type, [(Type, [DscStLyt.BindingType])], [Type])

type family CreateInfoArgs4ToCArgs1 (cia :: CreateInfoArgs4) :: CArgs1 where
	CreateInfoArgs4ToCArgs1 '(n, nnskndscdvss, slsbtss, sbph) = slsbtss

type family
	CreateInfoListArgs4ToCArgs1 (cias :: [CreateInfoArgs4]) :: [CArgs1] where
	CreateInfoListArgs4ToCArgs1 '[] = '[]
	CreateInfoListArgs4ToCArgs1 (cia ': cias) =
		CreateInfoArgs4ToCArgs1 cia ':
		CreateInfoListArgs4ToCArgs1 cias

createInfoToMiddle :: (Pokable n', Pokable c) =>
	Device.D ds ->
	CreateInfo n '(n1, n', 'GlslComputeShader, c, d, vs) slsbtss sbph ->
	IO (M.CreateInfo n n1 vs)
createInfoToMiddle dvc CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoStage = stg,
	createInfoLayout = U3 (Layout.L lyt),
	createInfoBasePipelineHandleOrIndex = mbphi } = do
	stg' <- ShaderStage.createInfoToMiddleFooNew dvc stg
	let	(bph, bpi) = case mbphi of
			Nothing -> (Nothing, Nothing)
			Just (Left (U2 (CNew h))) -> (Just h, Nothing)
			Just (Right i) -> (Nothing, Just i)
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
		Device.D sd -> HeteroParList.PL (U4 CreateInfo) as ->
		IO (HeteroParList.PL (U3 M.CreateInfo) (Result as))

instance CreateInfoListToMiddle '[] where
	type Result '[] = '[]
	createInfoListToMiddle _ HeteroParList.Nil = pure HeteroParList.Nil

instance (Pokable n', Pokable c, CreateInfoListToMiddle as) =>
	CreateInfoListToMiddle (
		'(n, '(n1, n', 'GlslComputeShader, c, d, vs), slsbtss, sbph
		) ': as) where
	type Result (
		'(n, '(n1, n', 'GlslComputeShader, c, d, vs), slsbtss, sbph) ':
		as ) = '(n, n1, vs) ': Result as
	createInfoListToMiddle dvc (U4 ci :** cis) = (:**)
		<$> (U3 <$> createInfoToMiddle dvc ci)
		<*> createInfoListToMiddle dvc cis

destroyCreateInfoMiddle :: Pokable d =>
	Device.D sd ->
	M.CreateInfo n n1 vs -> CreateInfo n '(n1, n2, 'GlslComputeShader, c, d, vs) slsbtss sbph -> IO ()
destroyCreateInfoMiddle dvc mci ci = ShaderStage.destroyCreateInfoMiddleNew dvc
	(M.createInfoStage mci) ((\(U6 s) -> s) $ createInfoStage ci)

class DestroyCreateInfoMiddleList vss vss' where
	destroyCreateInfoMiddleList ::
		Device.D sd ->
		HeteroParList.PL (U3 M.CreateInfo) vss ->
		HeteroParList.PL (U4 CreateInfo) vss' -> IO ()

instance DestroyCreateInfoMiddleList '[] '[] where
	destroyCreateInfoMiddleList _ HeteroParList.Nil HeteroParList.Nil = pure ()

instance (Pokable d, DestroyCreateInfoMiddleList vss vss') =>
	DestroyCreateInfoMiddleList
		('(n, n1, vs) ': vss)
		('(n, '(n1, n2, 'GlslComputeShader, c, d, vs), slsbtss, sbph) ': vss') where
	destroyCreateInfoMiddleList dvc
		(U3 mci :** mcis) (U4 ci :** cis) = do
		destroyCreateInfoMiddle dvc mci ci
		destroyCreateInfoMiddleList dvc mcis cis

createCs :: (
	CreateInfoListToMiddle vss, M.CreateInfoListToCore (Result vss),
	Pokable c', Pokable d',
	DestroyCreateInfoMiddleList (Result vss) vss,
	HeteroParList.HomoList '() (HeteroParList.ToDummies vss) ) =>
	Device.D sd -> Maybe Cache.C -> HeteroParList.PL (U4 CreateInfo) vss ->
	Maybe (AllocationCallbacks.A c') -> Maybe (AllocationCallbacks.A d') ->
	(forall s . HeteroParList.LL (C s) (HeteroParList.ToDummies vss) -> IO a) -> IO a
createCs dvc@(Device.D mdvc) cch cis macc macd f = do
	cis' <- createInfoListToMiddle dvc cis
	bracket
		(M.createCs mdvc cch cis' macc
			<* destroyCreateInfoMiddleList dvc cis' cis)
		(mapM_ \c -> M.destroy mdvc c macd)
		(f . HeteroParList.homoListFromList @_ @'() . (HeteroParList.Dummy . C <$>))

createCsNew :: (
	CreateInfoListToMiddle vss, M.CreateInfoListToCore (Result vss),
	Pokable c', Pokable d',
	DestroyCreateInfoMiddleList (Result vss) vss,
	HeteroParList.HomoList '() (HeteroParList.ToDummies vss),
	FromMiddleList (CreateInfoListArgs4ToCArgs1 vss) ) =>
	Device.D sd -> Maybe Cache.C -> HeteroParList.PL (U4 CreateInfo) vss ->
	Maybe (AllocationCallbacks.A c') -> Maybe (AllocationCallbacks.A d') ->
	(forall s . HeteroParList.PL (CNew s) (CreateInfoListArgs4ToCArgs1 vss) -> IO a) -> IO a
createCsNew dvc@(Device.D mdvc) cch cis macc macd f = do
	cis' <- createInfoListToMiddle dvc cis
	bracket
		(M.createCs mdvc cch cis' macc
			<* destroyCreateInfoMiddleList dvc cis' cis)
		(mapM_ \c -> M.destroy mdvc c macd)
		(f . fromMiddleList)

class FromMiddleList ss where
	fromMiddleList :: [M.C] -> HeteroParList.PL (CNew sc) ss

instance FromMiddleList '[] where fromMiddleList [] = HeteroParList.Nil

instance FromMiddleList ss => FromMiddleList (s ': ss) where
	fromMiddleList (c : cs) = CNew c :** fromMiddleList cs
