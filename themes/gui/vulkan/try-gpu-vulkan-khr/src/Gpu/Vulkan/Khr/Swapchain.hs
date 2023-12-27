{-# LANGUAGE ImportQualifiedPost, PackageImports #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr.Swapchain (

	-- * EXTENSION NAME

	extensionName,

	-- * CREATE

	create, unsafeRecreate, S, CreateInfo(..),

	-- ** Group

	group, Group, create', unsafeDestroy, lookup,

	-- * GET IMAGES

	getImages ) where

import Prelude hiding (lookup)
import Foreign.Storable.PeekPoke
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Control.Exception
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.Tuple.Uncurry
import Data.Map qualified as Map
import Data.Word

import Gpu.Vulkan
import Gpu.Vulkan.Khr.Enum
import Gpu.Vulkan.Khr.Swapchain.Type
import Gpu.Vulkan.Khr.Swapchain.Enum
import Gpu.Vulkan.Khr.Surface.Enum

import qualified Gpu.Vulkan as C
import qualified Gpu.Vulkan.TypeEnum as T
import qualified Gpu.Vulkan.AllocationCallbacks.Internal as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Internal as Device
import qualified Gpu.Vulkan.Image.Internal as Image
import qualified Gpu.Vulkan.Khr.Swapchain.Middle as M

import qualified Gpu.Vulkan.Image as Image
import qualified Gpu.Vulkan.QueueFamily as QueueFamily
import qualified Gpu.Vulkan.Khr.Surface.Type as Surface

import Gpu.Vulkan.PhysicalDevice qualified as PhysicalDevice

extensionName :: PhysicalDevice.ExtensionName
extensionName = PhysicalDevice.ExtensionName M.extensionName

create :: (
	WithPoked (TMaybe.M mn), T.FormatToValue fmt,
	AllocationCallbacks.ToMiddle mac ) =>
	Device.D sd -> CreateInfo mn ssfc fmt ->
	TPMaybe.M (U2 AllocationCallbacks.A) mac ->
	(forall s . S fmt s -> IO a) -> IO a
create (Device.D dvc) ci (AllocationCallbacks.toMiddle -> mac) f = bracket
	(M.create dvc (createInfoToMiddle ci) mac)
	(\sc -> M.destroy dvc sc mac) (f . S)

unsafeRecreate :: (
	WithPoked (TMaybe.M mn), T.FormatToValue fmt,
	AllocationCallbacks.ToMiddle mac ) =>
	Device.D sd -> CreateInfo mn ssfc fmt ->
	TPMaybe.M (U2 AllocationCallbacks.A) mac -> S fmt ssc -> IO ()
unsafeRecreate (Device.D dvc) ci (AllocationCallbacks.toMiddle -> mac) (S sc) =
	M.recreate dvc (createInfoToMiddle ci) mac sc

data CreateInfo mn ssfc (fmt :: T.Format) = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: CreateFlags,
	createInfoSurface :: Surface.S ssfc,
	createInfoMinImageCount :: Word32,
	createInfoImageColorSpace :: ColorSpace,
	createInfoImageExtent :: C.Extent2d,
	createInfoImageArrayLayers :: Word32,
	createInfoImageUsage :: Image.UsageFlags,
	createInfoImageSharingMode :: SharingMode,
	createInfoQueueFamilyIndices :: [QueueFamily.Index],
	createInfoPreTransform :: TransformFlagBits,
	createInfoCompositeAlpha :: CompositeAlphaFlagBits,
	createInfoPresentMode :: PresentMode,
	createInfoClipped :: Bool,
	createInfoOldSwapchain :: Maybe M.S }

deriving instance Show (TMaybe.M mn) => Show (CreateInfo mn ss fmt)

createInfoToMiddle :: forall n ss fmt . T.FormatToValue fmt =>
	CreateInfo n ss fmt -> M.CreateInfo n
createInfoToMiddle CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoSurface = Surface.S sfc,
	createInfoMinImageCount = mic,
	createInfoImageColorSpace = cs,
	createInfoImageExtent = ext,
	createInfoImageArrayLayers = ials,
	createInfoImageUsage = usg,
	createInfoImageSharingMode = sm,
	createInfoQueueFamilyIndices = qfis,
	createInfoPreTransform = ptfm,
	createInfoCompositeAlpha = calph,
	createInfoPresentMode = pm,
	createInfoClipped = clpd,
	createInfoOldSwapchain = osc } = M.CreateInfo {
	M.createInfoNext = mnxt,
	M.createInfoFlags = flgs,
	M.createInfoSurface = sfc,
	M.createInfoMinImageCount = mic,
	M.createInfoImageFormat = T.formatToValue @fmt,
	M.createInfoImageColorSpace = cs,
	M.createInfoImageExtent = ext,
	M.createInfoImageArrayLayers = ials,
	M.createInfoImageUsage = usg,
	M.createInfoImageSharingMode = sm,
	M.createInfoQueueFamilyIndices = qfis,
	M.createInfoPreTransform = ptfm,
	M.createInfoCompositeAlpha = calph,
	M.createInfoPresentMode = pm,
	M.createInfoClipped = clpd,
	M.createInfoOldSwapchain = osc }

getImages :: Device.D sd -> S fmt ss -> IO [Image.Binded ss ss nm fmt]
getImages (Device.D d) (S sc) = (Image.Binded <$>) <$> M.getImages d sc

data Group sd ma fmt ssc k = Group (Device.D sd)
	(TPMaybe.M (U2 AllocationCallbacks.A) ma) TSem (TVar (Map.Map k (S fmt ssc)))

group :: forall fmt k sd ma a .
	AllocationCallbacks.ToMiddle ma =>
	Device.D sd -> TPMaybe.M (U2 AllocationCallbacks.A) ma ->
	(forall ssc . Group sd ma fmt ssc k -> IO a) -> IO a
group dvc@(Device.D mdvc) mac@(AllocationCallbacks.toMiddle -> mmac) f = do
	(sem, m) <- atomically $ (,) <$> newTSem 1 <*> newTVar Map.empty
	rtn <- f $ Group dvc mac sem m
	((\(S s) -> M.destroy mdvc s mmac) `mapM_`) =<< atomically (readTVar m)
	pure rtn

create' :: (
	T.FormatToValue fmt,
	Ord k, WithPoked (TMaybe.M mn), AllocationCallbacks.ToMiddle ma ) =>
	Group sd ma fmt ss k -> k -> CreateInfo mn ssfc fmt ->
	IO (Either String (S fmt ss))
create' (Group (Device.D mdvc)
	(AllocationCallbacks.toMiddle -> mmac) sem ss) k
	(createInfoToMiddle -> mci) = do
	ok <- atomically do
		mx <- Map.lookup k <$> readTVar ss
		case mx of
			Nothing -> waitTSem sem >> pure True
			Just _ -> pure False
	if ok
	then do	s <- M.create mdvc mci mmac
		let	s' = S s
		atomically $ modifyTVar ss (Map.insert k s') >> signalTSem sem
		pure $ Right s'
	else pure . Left $
		"Gpu.Vulkan.Khr.Swapchain.create': The key already exist"

unsafeDestroy :: (Ord k, AllocationCallbacks.ToMiddle ma) =>
	Group sd ma fmt ssc k -> k -> IO (Either String ())
unsafeDestroy (Group (Device.D mdvc)
	(AllocationCallbacks.toMiddle -> ma) sem scs) k = do
	msc <- atomically do
		mx <- Map.lookup k <$> readTVar scs
		case mx of
			Nothing -> pure Nothing
			Just _ -> waitTSem sem >> pure mx
	case msc of
		Nothing -> pure $ Left
			"Gpu.Vulkan.Khr.Swapchain.destroy: No such key"
		Just (S sc) -> do
			M.destroy mdvc sc ma
			atomically do
				modifyTVar scs $ Map.delete k
				signalTSem sem
				pure $ Right ()

lookup :: Ord k => Group sd ma fmt ssc k -> k -> IO (Maybe (S fmt ssc))
lookup (Group _ _ _sem scs) k = atomically $ Map.lookup k <$> readTVar scs
