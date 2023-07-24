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

	-- CREATE

	createCsNew, C(..), CreateInfo(..), CreateInfoListToMiddle

	) where

import Foreign.Storable.PeekPoke
import Foreign.Storable.HeteroList
import Control.Exception
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.Tuple.Uncurry
import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:**))
import Data.Kind
import Data.Int

import Shaderc.EnumAuto

import Gpu.Vulkan.Pipeline.Enum

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.AllocationCallbacks.Type as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Pipeline.ShaderStage.Internal as ShaderStage
import qualified Gpu.Vulkan.DescriptorSetLayout.Type as DescriptorSetLayout
import qualified Gpu.Vulkan.PipelineLayout.Type as Layout
import qualified Gpu.Vulkan.PipelineCache.Type as Cache
import qualified Gpu.Vulkan.Pipeline.Compute.Middle as M

import Gpu.Vulkan.DescriptorSetLayout.Type qualified as DscStLyt

newtype C s (slbtss ::
	(Type, [(Type, [DescriptorSetLayout.BindingType])], [Type])) = C M.C
	deriving Show

data CreateInfo mn nncdvs slsbtss sbph = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: CreateFlags,
	createInfoStage :: U5 ShaderStage.CreateInfoNew nncdvs,
	createInfoLayout :: U3 Layout.L slsbtss,
	createInfoBasePipelineHandleOrIndex ::
		Maybe (Either (U2 C sbph) Int32) }

type CreateInfoArgs4 = (
	Maybe Type,
	(Maybe Type, Maybe Type, Shaderc.EnumAuto.ShaderKind, Maybe (Type, Type), [Type]),
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

createInfoToMiddle ::
	(WithPoked (TMaybe.M n'), AllocationCallbacks.ToMiddle mscc) =>
	Device.D ds ->
	CreateInfo n '(n1, n', 'GlslComputeShader, mscc, vs) slsbtss sbph ->
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
			Just (Left (U2 (C h))) -> (Just h, Nothing)
			Just (Right i) -> (Nothing, Just i)
	pure M.CreateInfo {
		M.createInfoNext = mnxt,
		M.createInfoFlags = flgs,
		M.createInfoStage = stg',
		M.createInfoLayout = lyt,
		M.createInfoBasePipelineHandle = bph,
		M.createInfoBasePipelineIndex = bpi }

class M.CreateInfoListToCore (Result as) => CreateInfoListToMiddle as where
	type Result as :: [(Maybe Type, Maybe Type, [Type])]
	createInfoListToMiddle ::
		Device.D sd -> HeteroParList.PL (U4 CreateInfo) as ->
		IO (HeteroParList.PL (U3 M.CreateInfo) (Result as))

instance CreateInfoListToMiddle '[] where
	type Result '[] = '[]
	createInfoListToMiddle _ HeteroParList.Nil = pure HeteroParList.Nil

instance (
	WithPoked (TMaybe.M n'), CreateInfoListToMiddle as,
	AllocationCallbacks.ToMiddle mscc,

	WithPoked (TMaybe.M n), WithPoked (TMaybe.M n1), PokableList vs
	) =>
	CreateInfoListToMiddle (
		'(n, '(n1, n', 'GlslComputeShader, mscc, vs), slsbtss, sbph
		) ': as) where
	type Result (
		'(n, '(n1, n', 'GlslComputeShader, mscc, vs), slsbtss, sbph) ':
		as ) = '(n, n1, vs) ': Result as
	createInfoListToMiddle dvc (U4 ci :** cis) = (:**)
		<$> (U3 <$> createInfoToMiddle dvc ci)
		<*> createInfoListToMiddle dvc cis

destroyCreateInfoMiddle ::
	AllocationCallbacks.ToMiddle mscc =>
	Device.D sd -> M.CreateInfo n n1 vs ->
	CreateInfo n '(n1, n2, 'GlslComputeShader, mscc, vs) slsbtss sbph -> IO ()
destroyCreateInfoMiddle dvc mci ci = ShaderStage.destroyCreateInfoMiddleNew dvc
	(M.createInfoStage mci) ((\(U5 s) -> s) $ createInfoStage ci)

class DestroyCreateInfoMiddleList vss vss' where
	destroyCreateInfoMiddleList ::
		Device.D sd ->
		HeteroParList.PL (U3 M.CreateInfo) vss ->
		HeteroParList.PL (U4 CreateInfo) vss' -> IO ()

instance DestroyCreateInfoMiddleList '[] '[] where
	destroyCreateInfoMiddleList _ HeteroParList.Nil HeteroParList.Nil = pure ()

instance (
	DestroyCreateInfoMiddleList vss vss',
	AllocationCallbacks.ToMiddle mscc ) =>
	DestroyCreateInfoMiddleList
		('(n, n1, vs) ': vss)
		('(n, '(n1, n2, 'GlslComputeShader, mscc, vs), slsbtss, sbph) ': vss') where
	destroyCreateInfoMiddleList dvc
		(U3 mci :** mcis) (U4 ci :** cis) = do
		destroyCreateInfoMiddle dvc mci ci
		destroyCreateInfoMiddleList dvc mcis cis

createCsNew :: (
	CreateInfoListToMiddle vss,
	DestroyCreateInfoMiddleList (Result vss) vss,
	HeteroParList.HomoList '() (HeteroParList.ToDummies vss),
	FromMiddleList (CreateInfoListArgs4ToCArgs1 vss),
	AllocationCallbacks.ToMiddle mscc' ) =>
	Device.D sd -> Maybe (Cache.C spc) -> HeteroParList.PL (U4 CreateInfo) vss ->
	TPMaybe.M (U2 AllocationCallbacks.A) mscc' ->
	(forall s . HeteroParList.PL (C s) (CreateInfoListArgs4ToCArgs1 vss) -> IO a) -> IO a
createCsNew dvc@(Device.D mdvc) mcch cis
	(AllocationCallbacks.toMiddle -> mac) f = do
	cis' <- createInfoListToMiddle dvc cis
	let	mcch' = (\(Cache.C c) -> c) <$> mcch
	bracket
		(M.createCs mdvc mcch' cis' mac
			<* destroyCreateInfoMiddleList dvc cis' cis)
		(mapM_ \c -> M.destroy mdvc c mac)
		(f . fromMiddleList)

class FromMiddleList ss where
	fromMiddleList :: [M.C] -> HeteroParList.PL (C sc) ss

instance FromMiddleList '[] where fromMiddleList [] = HeteroParList.Nil

instance FromMiddleList ss => FromMiddleList (s ': ss) where
	fromMiddleList (c : cs) = C c :** fromMiddleList cs
