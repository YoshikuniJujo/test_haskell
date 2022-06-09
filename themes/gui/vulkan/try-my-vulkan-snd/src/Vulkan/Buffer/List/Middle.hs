{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Buffer.List.Middle where

import Foreign.Storable
import Foreign.Pointable
import Data.Kind

import qualified Foreign.Storable.Generic

import Vulkan.Enum
import Vulkan.Buffer.Enum

import qualified Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Vulkan.Device.Middle as Device
import qualified Vulkan.Buffer.Middle as M
import qualified Vulkan.Buffer.Core as C
import qualified Vulkan.Memory.Middle as Memory.M
import qualified Vulkan.QueueFamily.EnumManual as QueueFamily

data CreateInfo n v = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoLength :: Int,
	createInfoUsage :: UsageFlags,
	createInfoSharingMode :: SharingMode,
	createInfoQueueFamilyIndices :: [QueueFamily.Index] }
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

bToMiddle :: B v -> M.B
bToMiddle (B b) = (M.B b)

create :: (
	Pointable n, Storable (Foreign.Storable.Generic.Wrap v),
	Pointable n' ) =>
	Device.D -> CreateInfo n v -> Maybe (AllocationCallbacks.A n') ->
	IO (B v)
create dvc ci = ((\(M.B b) -> B b) <$>) . M.create dvc (createInfoToMiddle ci)

destroy :: Pointable n =>
	Device.D -> B v -> Maybe (AllocationCallbacks.A n) -> IO ()
destroy dvc (B b) = M.destroy dvc $ M.B b

getMemoryRequirements :: Device.D -> B v -> IO Memory.M.Requirements
getMemoryRequirements dvc (B b) = M.getMemoryRequirements dvc $ M.B b

bindMemory :: Device.D -> B v -> Device.MemoryList v -> IO ()
bindMemory dvc (B b) (Device.MemoryList mem) =
	M.bindMemory dvc (M.B b) (Device.Memory mem) 0

data BList (vs :: [Type]) where
	BNil :: BList '[]
	(:!:) :: (B v, Device.Size) -> BList vs -> BList (v ': vs)

bListToMList :: BList vs -> [(M.B, Device.Size)]
bListToMList BNil = []
bListToMList ((b, sz) :!: bs) = (bToMiddle b, sz) : bListToMList bs

data Copy v = Copy { copyLength :: Int } deriving Show

copyToCore :: forall v .
	Storable (Foreign.Storable.Generic.Wrap v) => Copy v -> C.Copy
copyToCore Copy { copyLength = ln } = C.Copy {
	C.copySrcOffset = 0,
	C.copyDstOffset = 0,
	C.copySize = fromIntegral
		$ sizeOf @(Foreign.Storable.Generic.Wrap v) undefined * ln }
