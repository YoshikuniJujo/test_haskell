{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Buffer.List where

import Foreign.Storable
import Foreign.Pointable
import Data.Word

import qualified Foreign.Storable.Generic

import Vulkan.Enum
import Vulkan.Buffer.Enum

import qualified Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Vulkan.Device as Device
import qualified Vulkan.Buffer.Middle as M
import qualified Vulkan.Buffer.Core as C

data CreateInfo n v = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoLength :: Int,
	createInfoUsage :: UsageFlags,
	createInfoSharingMode :: SharingMode,
	createInfoQueueFamilyIndices :: [Word32] }
	deriving Show

{-
createInfoToCore :: forall n v r . (
	Storable (Foreign.Storable.Generic.Wrap v), Pointable n ) =>
	CreateInfo n v -> ContT r IO (Ptr C.CreateInfo)
createInfoToCore = M.createInfoToCore . createInfoToMiddle
-}

createInfoToMiddle :: forall n v . Storable (Foreign.Storable.Generic.Wrap v) =>
	CreateInfo n v -> M.CreateInfo n
createInfoToMiddle CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoLength = ln,
	createInfoUsage = usg,
	createInfoSharingMode = smd,
	createInfoQueueFamilyIndices = qfis
	} = M.CreateInfo {
		M.createInfoNext = mnxt,
		M.createInfoFlags = flgs,
		M.createInfoSize =
			fromIntegral $ ((sz - 1) `div` al + 1) * al * ln,
		M.createInfoUsage = usg,
		M.createInfoSharingMode = smd,
		M.createInfoQueueFamilyIndices = qfis }
	where
	sz = sizeOf @(Foreign.Storable.Generic.Wrap v) undefined
	al = alignment @(Foreign.Storable.Generic.Wrap v) undefined

newtype B v = B C.B deriving (Show, Storable)

create :: (
	Pointable n, Storable (Foreign.Storable.Generic.Wrap v),
	Pointable n' ) =>
	Device.D -> CreateInfo n v -> Maybe (AllocationCallbacks.A n') ->
	IO (B v)
create dvc ci = ((\(M.B b) -> B b) <$>) . M.create dvc (createInfoToMiddle ci)
