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

	-- * CREATE

	createCs, C(..), CreateInfo(..),
	CreateInfoListToMiddle, FromMiddleList

	) where

import Foreign.Storable.PeekPoke
import Foreign.Storable.HeteroList
import Control.Exception
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Tuple.MapIndex qualified as TMapIndex
import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:**))
import Data.Kind
import Data.Int

import Language.SpirV.ShaderKind

import Gpu.Vulkan.Pipeline.Enum

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.AllocationCallbacks.Type as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Pipeline.ShaderStage.Internal as ShaderStage
import qualified Gpu.Vulkan.DescriptorSetLayout.Type as DescriptorSetLayout
import qualified Gpu.Vulkan.PipelineLayout.Type as Layout
import qualified Gpu.Vulkan.PipelineCache.Type as Cache
import qualified Gpu.Vulkan.Pipeline.Compute.Middle as M


newtype C s (lyta ::
	(Type, [(Type, [DescriptorSetLayout.BindingType])], [Type])) = C M.C
	deriving Show

data CreateInfo mn ssta lyta bpha = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: CreateFlags,
	createInfoStage :: U5 ShaderStage.CreateInfo ssta,
	createInfoLayout :: U3 Layout.P lyta,
	createInfoBasePipelineHandleOrIndex ::
		Maybe (Either (U2 C bpha) Int32) }

createInfoToMiddle ::
	(WithPoked (TMaybe.M n'), AllocationCallbacks.ToMiddle mscc) =>
	Device.D ds ->
	CreateInfo n '(n1, n', 'GlslComputeShader, mscc, vs) lyta bpha ->
	IO (M.CreateInfo n n1 vs)
createInfoToMiddle dvc CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoStage = U5 stg,
	createInfoLayout = U3 (Layout.P lyt),
	createInfoBasePipelineHandleOrIndex = mbphi } = do
	stg' <- ShaderStage.createInfoToMiddle dvc stg
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

class (	M.CreateInfoListToCore (MiddleArgs as),
	DestroyCreateInfoMiddleList (MiddleArgs as) as ) =>
	CreateInfoListToMiddle as where
	type MiddleArgs as :: [(Maybe Type, Maybe Type, [Type])]
	createInfoListToMiddle ::
		Device.D sd -> HeteroParList.PL (U4 CreateInfo) as ->
		IO (HeteroParList.PL (U3 M.CreateInfo) (MiddleArgs as))

instance CreateInfoListToMiddle '[] where
	type MiddleArgs '[] = '[]
	createInfoListToMiddle _ HeteroParList.Nil = pure HeteroParList.Nil

instance (
	WithPoked (TMaybe.M n'), CreateInfoListToMiddle as,
	AllocationCallbacks.ToMiddle mscc,

	WithPoked (TMaybe.M n), WithPoked (TMaybe.M n1), PokableList vs
	) =>
	CreateInfoListToMiddle (
		'(n, '(n1, n', 'GlslComputeShader, mscc, vs), lyta, bpha
		) ': as) where
	type MiddleArgs (
		'(n, '(n1, n', 'GlslComputeShader, mscc, vs), lyta, bpha) ':
		as ) = '(n, n1, vs) ': MiddleArgs as
	createInfoListToMiddle dvc (U4 ci :** cis) = (:**)
		<$> (U3 <$> createInfoToMiddle dvc ci)
		<*> createInfoListToMiddle dvc cis

destroyCreateInfoMiddle ::
	AllocationCallbacks.ToMiddle mscc =>
	Device.D sd -> M.CreateInfo n n1 vs ->
	CreateInfo n '(n1, n2, 'GlslComputeShader, mscc, vs) lyta bpha -> IO ()
destroyCreateInfoMiddle dvc mci ci = ShaderStage.destroyShaderModule dvc
	(M.createInfoStage mci) ((\(U5 s) -> snd $ ShaderStage.createInfoModule s) $ createInfoStage ci)

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
		('(n, '(n1, n2, 'GlslComputeShader, mscc, vs), lyta, bpha) ': vss') where
	destroyCreateInfoMiddleList dvc
		(U3 mci :** mcis) (U4 ci :** cis) = do
		destroyCreateInfoMiddle dvc mci ci
		destroyCreateInfoMiddleList dvc mcis cis

createCs :: (
	CreateInfoListToMiddle cias, AllocationCallbacks.ToMiddle mac,
	FromMiddleList (TMapIndex.M2_4 cias) ) =>
	Device.D sd -> Maybe (Cache.P spc) -> HeteroParList.PL (U4 CreateInfo) cias ->
	TPMaybe.M (U2 AllocationCallbacks.A) mac ->
	(forall s . HeteroParList.PL (C s) (TMapIndex.M2_4 cias) -> IO a) -> IO a
createCs dvc@(Device.D mdvc) mcch cis
	(AllocationCallbacks.toMiddle -> mac) f = do
	cis' <- createInfoListToMiddle dvc cis
	let	mcch' = (\(Cache.P c) -> c) <$> mcch
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
