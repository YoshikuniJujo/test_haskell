{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.Graphics (

	-- * CREATE AND RECREATE

	createGs, unsafeRecreateGs, G, CreateInfo(..),
	CreateInfoListArgsToGArgs, CreateInfoListToMiddle,

	-- ** Group

	group, Group, createGs', unsafeDestroyGs, lookup

	) where

import Prelude hiding (lookup)
import Foreign.Storable.PeekPoke
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Control.Exception
import Data.Kind
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Tuple.Index qualified as TIndex
import Data.TypeLevel.Tuple.MapIndex qualified as TMapIndex
import Data.Map qualified as Map
import Data.HeteroParList qualified as HeteroParList
import Data.HeteroParList (pattern (:**))
import Data.HeteroParList.Tuple qualified as HeteroParList
import Data.Word
import Data.Int

import Language.SpirV.ShaderKind

import Gpu.Vulkan.Pipeline.Enum
import Gpu.Vulkan.Pipeline.Graphics.Type

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.AllocationCallbacks.Type as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Pipeline.Graphics.Middle as M
import qualified Gpu.Vulkan.PipelineCache.Type as Cache
import qualified Gpu.Vulkan.PipelineLayout.Type as Layout

import qualified Gpu.Vulkan.RenderPass.Type as RenderPass
import qualified Gpu.Vulkan.Pipeline.DynamicState as DynamicState
import qualified Gpu.Vulkan.Pipeline.ColorBlendState as ColorBlendState
import qualified Gpu.Vulkan.Pipeline.DepthStencilState as DepthStencilState
import qualified Gpu.Vulkan.Pipeline.MultisampleState as MultisampleState
import qualified Gpu.Vulkan.Pipeline.RasterizationState as RasterizationState
import qualified Gpu.Vulkan.Pipeline.ViewportState as ViewportState
import qualified Gpu.Vulkan.Pipeline.TessellationState as TessellationState
import qualified Gpu.Vulkan.Pipeline.InputAssemblyState as InputAssemblyState
import Gpu.Vulkan.Pipeline.VertexInputState.Internal
	qualified as VertexInputState
import qualified Gpu.Vulkan.Pipeline.ShaderStage.Internal as ShaderStage

import Gpu.Vulkan.VertexInput qualified as VertexInput

import qualified Gpu.Vulkan.Pipeline.ShaderStage.Middle as ShaderStage.M

-- CREATE AND RECREATE

createGs :: (CreateInfoListToMiddle cias, AllocationCallbacks.ToMiddle mac) =>
	Device.D sd -> Maybe (Cache.P sc) ->
	HeteroParList.PL (U14 CreateInfo) cias ->
	TPMaybe.M (U2 AllocationCallbacks.A) mac ->
	(forall sg .
		HeteroParList.PL
			(U3 (G sg)) (CreateInfoListArgsToGArgs cias) ->
		IO a) -> IO a
createGs d@(Device.D dvc) ((Cache.pToMiddle <$>) -> mc) cis
	macc@(AllocationCallbacks.toMiddle -> macd) f = bracket
	(createInfoListToMiddle d cis >>= \cis' -> M.createGs dvc mc cis'
			(AllocationCallbacks.toMiddle macc)
		<* destroyShaderStages' d cis' (allocationCallbacksListListFromCreateInfoList cis))
	(\gs -> M.destroyGs dvc gs macd) (f . gListFromMiddle)

class GListFromMiddle ss where
	gListFromMiddle :: [M.G] -> HeteroParList.PL (U3 (G sg)) ss
instance GListFromMiddle '[] where
	gListFromMiddle = \case
		[] -> HeteroParList.Nil
		_ : _ -> error "Wrong number of elements"

instance GListFromMiddle ss => GListFromMiddle ('(s, t, foo) ': ss) where
	gListFromMiddle = \case
		g : gs -> U3 (G @_ @s @t g) :** gListFromMiddle @ss gs
		[] -> error "Wrong number of elements"

unsafeRecreateGs :: (CreateInfoListToMiddle cias, AllocationCallbacks.ToMiddle mac) =>
	Device.D sd -> Maybe (Cache.P s) -> HeteroParList.PL (U14 CreateInfo) cias ->
	TPMaybe.M (U2 AllocationCallbacks.A) mac ->
	HeteroParList.PL (U3 (G sg)) (CreateInfoListArgsToGArgs cias) -> IO ()
unsafeRecreateGs d@(Device.D dvc) ((Cache.pToMiddle <$>) -> mc) cis macc gpls = do
	cis' <- createInfoListToMiddle d cis
	M.recreateGs dvc mc cis'
		(AllocationCallbacks.toMiddle macc) $ gListToMiddle gpls
	destroyShaderStages' d cis' $ allocationCallbacksListListFromCreateInfoList cis

gListToMiddle :: HeteroParList.PL (U3 (G sg)) ss -> [M.G]
gListToMiddle = HeteroParList.toList $ \(U3 (G g)) -> g

-- CREATE INFO

data CreateInfo
	mn ssas visa iasa tsssa vpsa rstsa mssa dssa cbsa dsa lyta rpa bpha =
	CreateInfo {
		createInfoNext :: TMaybe.M mn,
		createInfoFlags :: CreateFlags,
		createInfoStages ::
			HeteroParList.PL (U5 ShaderStage.CreateInfo) ssas,
		createInfoVertexInputState ::
			Maybe (U3 VertexInputState.CreateInfo visa),
		createInfoInputAssemblyState ::
			Maybe (InputAssemblyState.CreateInfo iasa),
		createInfoTessellationState ::
			Maybe (TessellationState.CreateInfo tsssa),
		createInfoViewportState :: Maybe (ViewportState.CreateInfo vpsa),
		createInfoRasterizationState ::
			Maybe (RasterizationState.CreateInfo rstsa),
		createInfoMultisampleState ::
			Maybe (MultisampleState.CreateInfo mssa),
		createInfoDepthStencilState ::
			Maybe (DepthStencilState.CreateInfo dssa),
		createInfoColorBlendState ::
			Maybe (ColorBlendState.CreateInfo cbsa),
		createInfoDynamicState :: Maybe (DynamicState.CreateInfo dsa),
		createInfoLayout :: U3 Layout.P lyta,
		createInfoRenderPass :: RenderPass.R rpa,
		createInfoSubpass :: Word32,
		createInfoBasePipelineHandle :: Maybe (U4 G bpha),
		createInfoBasePipelineIndex :: Int32 }

-- CREATE INFO LIST ARGS TO G ARGS

type family CreateInfoListArgsToGArgs cias where
	CreateInfoListArgsToGArgs '[] = '[]
	CreateInfoListArgsToGArgs (cia ': cias) =
		CreateInfoArgsToGArgs cia ': CreateInfoListArgsToGArgs cias

type family CreateInfoArgsToGArgs cia where
	CreateInfoArgsToGArgs '(
		n, ssas, '(nv, vibs, vias), iasa, tsssa, vpsa, rstsa, mssa, dssa, cbsa, dsa,
		slbtss, rpa, bpha ) = '(vibs, vias, slbtss)

-- CREATE INFO LIST TO MIDDLE

class (	GListFromMiddle (CreateInfoListArgsToGArgs cias),
	DestroyShaderStages
		(MiddleArgs cias) (TAllocationCallbacksListList cias),
	AllocationCallbacksListListFromCreateInfoList cias,
	M.CreateInfoListToCore (MiddleArgs cias) ) =>
	CreateInfoListToMiddle cias where
	type MiddleArgs cias :: [(
		Maybe Type, [(Maybe Type, ShaderKind, [Type])], Maybe Type,
		Maybe Type, Maybe Type, Maybe Type, Maybe Type, Maybe Type,
		Maybe Type, Maybe Type, Maybe Type)]
	createInfoListToMiddle ::
		Device.D sd -> HeteroParList.PL (U14 CreateInfo) cias ->
		IO (HeteroParList.PL (U11 M.CreateInfo) (MiddleArgs cias))

instance CreateInfoListToMiddle '[] where
	type MiddleArgs '[] = '[]
	createInfoListToMiddle _ HeteroParList.Nil = pure HeteroParList.Nil

instance (

	HeteroParList.Map3_5 ssas,
	ShaderStage.CreateInfoListToMiddle ssas,
	ShaderStage.M.CreateInfoListToCore (ShaderStage.MiddleArgs ssas),
	VertexInputState.BindingStrideList vibs VertexInput.Rate,
	VertexInputState.AttributeDescriptions vibs vias,
	WithPoked (TMaybe.M mn), WithPoked (TMaybe.M nvis),
	WithPoked (TMaybe.M iasa), WithPoked (TMaybe.M tsssa),
	WithPoked (TMaybe.M vpsa), WithPoked (TMaybe.M rstsa),
	WithPoked (TMaybe.M mssa), WithPoked (TMaybe.M dssa),
	WithPoked (TMaybe.M cbsa), WithPoked (TMaybe.M dsa),
	CreateInfoListToMiddle ss ) =>
	CreateInfoListToMiddle ('(
		mn, ssas, '(nvis, vibs, vias), iasa, tsssa, vpsa, rstsa, mssa, dssa, cbsa, dsa,
		'(sl, sbtss, pcl), rpa, '(sb, vibs', vias', slbtss') ) ': ss)  where
	type MiddleArgs ('(
		mn, ssas, '(nvis, vibs, vias), iasa, tsssa, vpsa, rstsa, mssa, dssa, cbsa, dsa,
		'(sl, sbtss, pcl), rpa, '(sb, vibs', vias', slbtss') ) ': ss) =
		'(mn, ShaderStage.MiddleArgs ssas, nvis, iasa, tsssa, vpsa, rstsa,
			mssa, dssa, cbsa, dsa) ': MiddleArgs ss
	createInfoListToMiddle dvc (U14 ci :** cis) = (:**)
		<$> (U11 <$> createInfoToMiddle dvc ci)
		<*> createInfoListToMiddle dvc cis

createInfoToMiddle :: (
	ShaderStage.CreateInfoListToMiddle ssas,
	VertexInputState.BindingStrideList vibs VertexInput.Rate,
	VertexInputState.AttributeDescriptions vibs vias ) =>
	Device.D sd ->
	CreateInfo n ssas '(nvis, vibs, vias)
		iasa tsssa vpsa rstsa mssa dssa cbsa dsa sl rpa '(sb, vibs', vias', slbtss') ->
	IO (M.CreateInfo n (ShaderStage.MiddleArgs ssas)
		nvis iasa tsssa vpsa rstsa mssa dssa cbsa dsa)
createInfoToMiddle dvc CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoStages = stgs,
	createInfoVertexInputState = vis,
	createInfoInputAssemblyState = ias,
	createInfoTessellationState = ts,
	createInfoViewportState = vs,
	createInfoRasterizationState = rs,
	createInfoMultisampleState = ms,
	createInfoDepthStencilState = dss,
	createInfoColorBlendState = cbs,
	createInfoDynamicState = ds,
	createInfoLayout = U3 (Layout.P lyt),
	createInfoRenderPass = RenderPass.R rp,
	createInfoSubpass = sp,
	createInfoBasePipelineHandle = bph,
	createInfoBasePipelineIndex = bpi } = do
	stgs' <- ShaderStage.createInfoListToMiddle dvc stgs
	bph' <- maybe M.gNull (\(U4 (G g)) -> pure g) bph
	pure M.CreateInfo {
		M.createInfoNext = mnxt,
		M.createInfoFlags = flgs,
		M.createInfoStages = stgs',
		M.createInfoVertexInputState =
			VertexInputState.createInfoToMiddle . unU3 <$> vis,
		M.createInfoInputAssemblyState = ias,
		M.createInfoTessellationState = ts,
		M.createInfoViewportState = vs,
		M.createInfoRasterizationState = rs,
		M.createInfoMultisampleState = ms,
		M.createInfoDepthStencilState = dss,
		M.createInfoColorBlendState = cbs,
		M.createInfoDynamicState = ds,
		M.createInfoLayout = lyt,
		M.createInfoRenderPass = rp,
		M.createInfoSubpass = sp,
		M.createInfoBasePipelineHandle = bph',
		M.createInfoBasePipelineIndex = bpi }

-- DESTROY SHADER STAGES

class DestroyShaderStages mvs macs where
	destroyShaderStages' :: Device.D sd ->
		HeteroParList.PL (U11 M.CreateInfo) mvs ->
		HeteroParList.PL2
			(TPMaybe.M (U2 AllocationCallbacks.A)) macs -> IO ()

instance DestroyShaderStages '[] '[] where
	destroyShaderStages' _ HeteroParList.Nil HeteroParList.Nil = pure ()

instance (ShaderStage.DestroyShaderModuleList a2 mac, DestroyShaderStages ss macs) =>
	DestroyShaderStages ('(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) ': ss) (mac ': macs) where
	destroyShaderStages' dvc (U11 cim :** cims) (mac :** macs) = do
		ShaderStage.destroyShaderModuleList dvc (M.createInfoStages cim) mac
		destroyShaderStages' dvc cims macs

class AllocationCallbacksListListFromCreateInfoList cias where
	type TAllocationCallbacksListList cias :: [[Maybe (Type, Type)]]
	allocationCallbacksListListFromCreateInfoList ::
		HeteroParList.PL (U14 CreateInfo) cias ->
		HeteroParList.PL2
			(TPMaybe.M (U2 AllocationCallbacks.A))
			(TAllocationCallbacksListList cias)

instance AllocationCallbacksListListFromCreateInfoList '[] where
	type TAllocationCallbacksListList '[] = '[]
	allocationCallbacksListListFromCreateInfoList
		HeteroParList.Nil = HeteroParList.Nil

instance (
	HeteroParList.Map3_5 (TIndex.I1_14 cia),
	AllocationCallbacksListListFromCreateInfoList cias ) =>
	AllocationCallbacksListListFromCreateInfoList (cia ': cias) where
	type TAllocationCallbacksListList (cia ': cias) =
		TMapIndex.M3_5 (TIndex.I1_14 cia) ':
		TAllocationCallbacksListList cias
	allocationCallbacksListListFromCreateInfoList (U14 ci :** cis) =
		allocationCallbacksListFromCreateInfo ci :**
		allocationCallbacksListListFromCreateInfoList cis

allocationCallbacksListFromCreateInfo :: HeteroParList.Map3_5 ssas =>
	CreateInfo mn ssas visa iasa tsssa vpsa rstsa mssa dssa cbsa dsa lyta rpa bpha ->
	HeteroParList.PL (TPMaybe.M (U2 AllocationCallbacks.A)) (TMapIndex.M3_5 ssas)
allocationCallbacksListFromCreateInfo =
	ShaderStage.allocationCallbacksListFromCreateInfoList . createInfoStages

-- Group

data Group sd ma sg k gas = Group (Device.D sd)
	(TPMaybe.M (U2 AllocationCallbacks.A) ma)
	TSem (TVar (Map.Map k ( HeteroParList.PL (U3 (G sg)) gas)))

group :: AllocationCallbacks.ToMiddle ma =>
	Device.D sd -> TPMaybe.M (U2 AllocationCallbacks.A) ma ->
	(forall sg . Group sd ma sg k gas -> IO a) -> IO a
group dvc@(Device.D mdvc) mac@(AllocationCallbacks.toMiddle -> mmac) f = do
	(sem, m) <- atomically $ (,) <$> newTSem 1 <*> newTVar Map.empty
	rtn <- f $ Group dvc mac sem m
	((\gs -> M.destroyGs mdvc (gListToMiddle gs) mmac) `mapM_`)
		=<< atomically (readTVar m)
	pure rtn

createGs' :: (
	Ord k,
	CreateInfoListToMiddle cias, AllocationCallbacks.ToMiddle mac ) =>
	Group sd mac sg k (CreateInfoListArgsToGArgs cias) -> k ->
	Maybe (Cache.P sc) ->
	HeteroParList.PL (U14 CreateInfo) cias ->
	IO (Either String
		(HeteroParList.PL (U3 (G sg)) (CreateInfoListArgsToGArgs cias)))
createGs' (Group d@(Device.D dvc)
	(AllocationCallbacks.toMiddle -> mmac) sem gss) k
	((Cache.pToMiddle <$>) -> mc) cis = do
	ok <- atomically do
		mx <- Map.lookup k <$> readTVar gss
		case mx of
			Nothing -> waitTSem sem >> pure True
			Just _ -> pure False
	if ok
	then do	gs <- createInfoListToMiddle d cis >>= \cis' -> M.createGs dvc mc cis' mmac
				<* destroyShaderStages' d cis'
					(allocationCallbacksListListFromCreateInfoList cis)
		let	gs' = gListFromMiddle gs
		atomically $ modifyTVar gss (Map.insert k gs') >> signalTSem sem
		pure $ Right gs'
	else pure . Left $
		"Gpu.Vulkan.Pipeline.Graphics.create' :: The key already exist"

unsafeDestroyGs :: (Ord k, AllocationCallbacks.ToMiddle ma) =>
	Group sd ma sg k gas -> k -> IO (Either String ())
unsafeDestroyGs (Group (Device.D mdvc)
	(AllocationCallbacks.toMiddle -> mma) sem gss) k = do
	mgs <- atomically do
		mx <- Map.lookup k <$> readTVar gss
		case mx of
			Nothing -> pure Nothing
			Just _ -> waitTSem sem >> pure mx
	case mgs of
		Nothing -> pure $ Left
			"Gpu.Vulkan.Pipeline.Graphics.unsafeDestroyGs: No such key"
		Just gs -> do
			atomically . modifyTVar gss $ Map.delete k
			M.destroyGs mdvc (gListToMiddle gs) mma
			Right <$> atomically (signalTSem sem)

lookup :: Ord k =>
	Group sd ma sg k gas -> k -> IO (Maybe (HeteroParList.PL (U3 (G sg)) gas))
lookup (Group _ _ _sem gss) k = atomically $ Map.lookup k <$> readTVar gss
