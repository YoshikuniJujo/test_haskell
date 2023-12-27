{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Framebuffer (

	-- * CREATE

	create, unsafeRecreate, F, CreateInfo(..),

	-- ** Group

	group, Group, create', unsafeDestroy, lookup,

	-- * ENUM

	module Gpu.Vulkan.Framebuffer.Enum

	) where

import Prelude hiding (lookup)
import Foreign.Storable.PeekPoke
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Control.Exception
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.HeteroParList qualified as HeteroParList
import Data.Map qualified as Map
import Data.Word

import Gpu.Vulkan.Framebuffer.Enum
import Gpu.Vulkan.Framebuffer.Type

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.AllocationCallbacks.Type as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.RenderPass.Type as RenderPass
import qualified Gpu.Vulkan.ImageView as ImageView
import qualified Gpu.Vulkan.ImageView.Type as ImageView
import qualified Gpu.Vulkan.Framebuffer.Middle as M

data CreateInfo mn sr aargs = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: CreateFlags,
	createInfoRenderPass :: RenderPass.R sr,
	createInfoAttachments :: HeteroParList.PL (U3 ImageView.I) aargs,
	createInfoWidth :: Word32,
	createInfoHeight :: Word32,
	createInfoLayers :: Word32 }

type family MapThird (t :: [(j, k, l)]) where
	MapThird '[] = '[]
	MapThird ('(a, b, c) ': abcs) = c ': MapThird abcs

createInfoToMiddle :: CreateInfo n sr fmtmnsis -> M.CreateInfo n
createInfoToMiddle CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoRenderPass = RenderPass.R rp,
	createInfoAttachments = HeteroParList.toList (\(U3 (ImageView.I iv)) -> iv) -> ivs,
	createInfoWidth = w,
	createInfoHeight = h,
	createInfoLayers = lyrs } = M.CreateInfo {
		M.createInfoNext = mnxt,
		M.createInfoFlags = flgs,
		M.createInfoRenderPass = rp,
		M.createInfoAttachments = ivs,
		M.createInfoWidth = w,
		M.createInfoHeight = h,
		M.createInfoLayers = lyrs }

create :: (WithPoked (TMaybe.M mn), AllocationCallbacks.ToMiddle mac) =>
	Device.D sd -> CreateInfo mn sr aargs ->
	TPMaybe.M (U2 AllocationCallbacks.A) mac ->
	(forall s . F s -> IO a) -> IO a
create (Device.D dvc) ci
	(AllocationCallbacks.toMiddle -> macc) f = bracket
	(M.create dvc (createInfoToMiddle ci) macc)
	(\fb -> M.destroy dvc fb macc) (f . F)

unsafeRecreate :: (WithPoked (TMaybe.M mn), AllocationCallbacks.ToMiddle mac) =>
	Device.D sd -> CreateInfo mn sr aargs ->
	TPMaybe.M (U2 AllocationCallbacks.A) mac ->
	F sf -> IO ()
unsafeRecreate (Device.D dvc) ci
	(AllocationCallbacks.toMiddle -> macc) (F fb) =
	M.recreate dvc (createInfoToMiddle ci) macc macc fb

data Group sd ma sf k = Group (Device.D sd)
	(TPMaybe.M (U2 AllocationCallbacks.A) ma) TSem (TVar (Map.Map k (F sf)))

group :: AllocationCallbacks.ToMiddle ma =>
	Device.D sd -> TPMaybe.M (U2 AllocationCallbacks.A) ma ->
	(forall sf . Group sd ma sf k -> IO a) -> IO a
group dvc@(Device.D mdvc) mac@(AllocationCallbacks.toMiddle -> mmac) f = do
	(sem, m) <- atomically $ (,) <$> newTSem 1 <*> newTVar Map.empty
	rtn <- f $ Group dvc mac sem m
	((\(F ff) -> M.destroy mdvc ff mmac) `mapM_`) =<< atomically (readTVar m)
	pure rtn

create' :: (
	Ord k, WithPoked (TMaybe.M mn), AllocationCallbacks.ToMiddle ma) =>
	Group sd ma sf k -> k -> CreateInfo mn sr aargs -> IO (Either String (F sf))
create' (Group (Device.D mdvc)
	(AllocationCallbacks.toMiddle -> mmac) sem sf) k
	(createInfoToMiddle -> ci) = do
	ok <- atomically do
		mx <- Map.lookup k <$> readTVar sf
		case mx of
			Nothing -> waitTSem sem >> pure True
			Just _ -> pure False
	if ok
	then do	f <- M.create mdvc ci mmac
		let	f' = F f
		atomically $ modifyTVar sf (Map.insert k f') >> signalTSem sem
		pure $ Right f'
	else pure . Left $
		"Gpu.Vulkan.Framebuffer.create': The key already exist"

unsafeDestroy :: (Ord k, AllocationCallbacks.ToMiddle ma) =>
	Group sd ma sf k -> k -> IO (Either String ())
unsafeDestroy (Group (Device.D mdvc)
	(AllocationCallbacks.toMiddle -> ma) sem fs) k = do
	mf <- atomically do
		mx <- Map.lookup k <$> readTVar fs
		case mx of
			Nothing -> pure Nothing
			Just _ -> waitTSem sem >> pure mx
	case mf of
		Nothing -> pure $ Left
			"Gpu.Vulkan.Framebuffer.unsafeDestroy: No such key"
		Just (F f) -> do
			M.destroy mdvc f ma
			atomically do
				modifyTVar fs $ Map.delete k
				signalTSem sem
				pure $ Right ()

lookup :: Ord k => Group sd ma sf k -> k -> IO (Maybe (F sf))
lookup (Group _ _ _sem fs) k = atomically $ Map.lookup k <$> readTVar fs
