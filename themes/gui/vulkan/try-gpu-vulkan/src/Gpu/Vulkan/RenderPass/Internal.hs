{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.RenderPass.Internal (

	-- * CREATE

	create, R, CreateInfo(..),

	-- ** Group

	group, Group, create', unsafeDestroy, lookup,

	-- * BEGIN INFO

	BeginInfo(..), beginInfoToMiddle

	) where

import Prelude hiding (lookup)

import Foreign.Storable.PeekPoke
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Control.Exception
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.Tuple.Uncurry
import Data.Map qualified as Map
import Data.HeteroParList qualified as HeteroParList

import Gpu.Vulkan.RenderPass.Type

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.AllocationCallbacks.Type as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import Gpu.Vulkan.RenderPass.Enum
import qualified Gpu.Vulkan.RenderPass.Middle as M
import qualified Gpu.Vulkan.Attachment as Attachment

import qualified Gpu.Vulkan.Subpass.Middle as Subpass

import qualified Gpu.Vulkan.Framebuffer.Type as Framebuffer
import Gpu.Vulkan.Middle

-- CREATE

create :: (
	WithPoked (TMaybe.M mn), Attachment.DescriptionListToMiddle fmts,
	AllocationCallbacks.ToMiddle mac ) =>
	Device.D sd -> CreateInfo mn fmts ->
	TPMaybe.M (U2 AllocationCallbacks.A) mac ->
	(forall s . R s -> IO a) -> IO a
create (Device.D dvc) ci (AllocationCallbacks.toMiddle -> mac) f = bracket
	(M.create dvc (createInfoToMiddle ci) mac)
	(\r -> M.destroy dvc r mac) (f . R)

data CreateInfo mn fmts = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: CreateFlags,
	createInfoAttachments ::
		HeteroParList.PL Attachment.Description fmts,
	createInfoSubpasses :: [Subpass.Description],
	createInfoDependencies :: [Subpass.Dependency] }

createInfoToMiddle :: Attachment.DescriptionListToMiddle fmts =>
	CreateInfo n fmts -> M.CreateInfo n
createInfoToMiddle CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoAttachments = atts,
	createInfoSubpasses = spss,
	createInfoDependencies = dps } = M.CreateInfo {
	M.createInfoNext = mnxt,
	M.createInfoFlags = flgs,
	M.createInfoAttachments = Attachment.descriptionListToMiddle atts,
	M.createInfoSubpasses = spss,
	M.createInfoDependencies = dps }

-- Group

data Group sd ma sr k = Group (Device.D sd)
	(TPMaybe.M (U2 AllocationCallbacks.A) ma)
	TSem (TVar (Map.Map k (R sr)))

group :: AllocationCallbacks.ToMiddle ma =>
	Device.D sd -> TPMaybe.M (U2 AllocationCallbacks.A) ma ->
	(forall sr . Group sd ma sr k -> IO a) -> IO a
group dvc@(Device.D mdvc) mac@(AllocationCallbacks.toMiddle -> mmac) f = do
	(sem, m) <- atomically $ (,) <$> newTSem 1 <*> newTVar Map.empty
	rtn <- f $ Group dvc mac sem m
	((\(R mr) -> M.destroy mdvc mr mmac) `mapM_`)
		=<< atomically (readTVar m)
	pure rtn

create' :: (
	Ord k, WithPoked (TMaybe.M mn), Attachment.DescriptionListToMiddle fmts,
	AllocationCallbacks.ToMiddle mac ) =>
	Group sd mac sr k -> k -> CreateInfo mn fmts ->
	IO (Either String (R sr))
create' (Group (Device.D mdvc)
	(AllocationCallbacks.toMiddle -> mac) sem rs) k ci = do
	ok <- atomically do
		mx <- Map.lookup k <$> readTVar rs
		case mx of
			Nothing -> waitTSem sem >> pure True
			Just _ -> pure False
	if ok
	then do	r <- M.create mdvc (createInfoToMiddle ci) mac
		let r' = R r
		atomically $ modifyTVar rs (Map.insert k r') >> signalTSem sem
		pure $ Right r'
	else pure . Left $
		"Gpu.Vulkan.RenderPass.Internal.create': The key already exist"

unsafeDestroy :: (Ord k, AllocationCallbacks.ToMiddle ma) =>
	Group sd ma sr k -> k -> IO (Either String ())
unsafeDestroy (Group (Device.D mdvc)
	(AllocationCallbacks.toMiddle -> ma) sem rs) k = do
	mr <- atomically do
		mx <- Map.lookup k <$> readTVar rs
		case mx of
			Nothing -> pure Nothing
			Just _ -> waitTSem sem >> pure mx
	case mr of
		Nothing -> pure $ Left
			"Gpu.Vulkan.RenderPass.Internal.unsafeDestroy: No such key"
		Just (R r) -> do
			M.destroy mdvc r ma
			atomically do
				modifyTVar rs $ Map.delete k
				signalTSem sem
				pure $ Right ()

lookup :: Ord k => Group sd ma sr k -> k -> IO (Maybe (R sr))
lookup (Group _ _ _sem rs) k = atomically $ Map.lookup k <$> readTVar rs

-- BEGIN INFO

data BeginInfo mn sr sf cts = BeginInfo {
	beginInfoNext :: TMaybe.M mn,
	beginInfoRenderPass :: R sr,
	beginInfoFramebuffer :: Framebuffer.F sf,
	beginInfoRenderArea :: Rect2d,
	beginInfoClearValues :: HeteroParList.PL ClearValue cts }

beginInfoToMiddle :: BeginInfo n sr sf cts -> M.BeginInfo n cts
beginInfoToMiddle BeginInfo {
	beginInfoNext = mnxt,
	beginInfoRenderPass = R rp,
	beginInfoFramebuffer = Framebuffer.F fb,
	beginInfoRenderArea = ra,
	beginInfoClearValues = cvs } = M.BeginInfo {
		M.beginInfoNext = mnxt,
		M.beginInfoRenderPass = rp,
		M.beginInfoFramebuffer = fb,
		M.beginInfoRenderArea = ra,
		M.beginInfoClearValues = cvs }
