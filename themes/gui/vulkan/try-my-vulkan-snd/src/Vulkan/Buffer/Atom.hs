{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Buffer.Atom where

import Foreign.Storable
import Foreign.Pointable
import Data.Word

import qualified Foreign.Storable.Generic

import Vulkan.Enum
import Vulkan.Buffer.Enum

import qualified Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Vulkan.Device.Middle as Device
import qualified Vulkan.Buffer.Middle as M
import qualified Vulkan.Buffer.Core as C
import qualified Vulkan.Memory.Middle as Memory.M

data CreateInfo n v = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoUsage :: UsageFlags,
	createInfoSharingMode :: SharingMode,
	createInfoQueueFamilyIndices :: [Word32] }
	deriving Show

createInfoToMiddle :: forall n v . Storable (Foreign.Storable.Generic.Wrap v) =>
	CreateInfo n v -> M.CreateInfo n
createInfoToMiddle CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoUsage = usg,
	createInfoSharingMode = smd,
	createInfoQueueFamilyIndices = qfis } = M.CreateInfo {
		M.createInfoNext = mnxt,
		M.createInfoFlags = flgs,
		M.createInfoSize = fromIntegral sz,
		M.createInfoUsage = usg,
		M.createInfoSharingMode = smd,
		M.createInfoQueueFamilyIndices = qfis }
	where sz = sizeOf @(Foreign.Storable.Generic.Wrap v) undefined

newtype B v = B C.B deriving (Show, Storable)

bToMiddle :: B v -> M.B
bToMiddle (B b) = (M.B b)

create :: (
	Pointable n, Storable (Foreign.Storable.Generic.Wrap v),
	Pointable n') =>
	Device.D -> CreateInfo n v -> Maybe (AllocationCallbacks.A n') ->
	IO (B v)
create dvc ci = ((\(M.B b) -> B b) <$>) . M.create dvc (createInfoToMiddle ci)

destroy :: Pointable n =>
	Device.D -> B v -> Maybe (AllocationCallbacks.A n) -> IO ()
destroy dvc (B b) = M.destroy dvc $ M.B b

getMemoryRequirements :: Device.D -> B v -> IO Memory.M.Requirements
getMemoryRequirements dvc (B b) = M.getMemoryRequirements dvc $ M.B b

bindMemory :: Device.D -> B v -> Device.MemoryAtom v -> IO ()
bindMemory dvc (B b) (Device.MemoryAtom mem) =
	M.bindMemory dvc (M.B b) (Device.Memory mem) 0
