{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Image (
	INew, BindedNew, createNew, recreateNew, M.CreateInfoNew(..),
	getMemoryRequirementsNew,

	I, Binded, create, M.CreateInfo(..), getMemoryRequirements, bindMemory,
	M.SubresourceRange(..), MemoryBarrier(..),

	memoryBarrierToMiddle, MemoryBarrierListToMiddle(..), FirstOfFives
	) where

import GHC.TypeLits
import Foreign.Pointable
import Control.Exception
import Data.Kind
import Data.HeteroList

import Gpu.Vulkan.Enum
import Gpu.Vulkan.Image.Type
import Gpu.Vulkan.Image.Enum hiding (Type)

import qualified Gpu.Vulkan.TypeEnum as T
import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.QueueFamily.EnumManual as QueueFamily
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Memory.Middle as Memory
import qualified Gpu.Vulkan.Image.Middle as M

createNew :: (Pointable n, Pointable n2, Pointable n3, T.FormatToValue fmt) =>
	Device.D sd -> M.CreateInfoNew n fmt ->
	Maybe (AllocationCallbacks.A n2) -> Maybe (AllocationCallbacks.A n3) ->
	(forall s . INew s nm fmt -> IO a) -> IO a
createNew (Device.D dvc) ci macc macd f =
	bracket (M.createNew dvc ci macc) (\i -> M.destroy dvc i macd) (f . INew)

recreateNew :: (
	T.FormatToValue fmt,
	Pointable n, Pointable c, Pointable d ) =>
	Device.D sd -> M.CreateInfoNew n fmt ->
	Maybe (AllocationCallbacks.A c) ->
	Maybe (AllocationCallbacks.A d) ->
	INew si nm fmt -> IO ()
recreateNew (Device.D dvc) ci macc macd (INew i) = M.recreateNew dvc ci macc macd i

create :: (Pointable n, Pointable n2, Pointable n3) =>
	Device.D sd -> M.CreateInfo n ->
	Maybe (AllocationCallbacks.A n2) -> Maybe (AllocationCallbacks.A n3) ->
	(forall s . I s -> IO a) -> IO a
create (Device.D dvc) ci macc macd f =
	bracket (M.create dvc ci macc) (\i -> M.destroy dvc i macd) (f . I)

getMemoryRequirementsNew :: Device.D sd -> INew s nm fmt -> IO Memory.Requirements
getMemoryRequirementsNew (Device.D dvc) (INew img) =
	M.getMemoryRequirements dvc img

getMemoryRequirements :: Device.D sd -> I s -> IO Memory.Requirements
getMemoryRequirements (Device.D dvc) (I img) = M.getMemoryRequirements dvc img

bindMemory :: Device.D sd -> I si -> Device.MemoryImage sm -> IO (Binded si sm)
bindMemory (Device.D dvc) (I img) (Device.MemoryImage _ mem) = do
	M.bindMemory dvc img mem 0
	pure $ Binded img

data MemoryBarrier n si sm nm fmt = MemoryBarrier {
	memoryBarrierNext :: Maybe n,
	memoryBarrierSrcAccessMask :: AccessFlags,
	memoryBarrierDstAccessMask :: AccessFlags,
	memoryBarrierOldLayout :: Layout,
	memoryBarrierNewLayout :: Layout,
	memoryBarrierSrcQueueFamilyIndex :: QueueFamily.Index,
	memoryBarrierDstQueueFamilyIndex :: QueueFamily.Index,
	memoryBarrierImage :: BindedNew si sm nm fmt,
	memoryBarrierSubresourceRange :: M.SubresourceRange }

memoryBarrierToMiddle :: MemoryBarrier n si sm nm fmt -> M.MemoryBarrier n
memoryBarrierToMiddle MemoryBarrier {
	memoryBarrierNext = mnxt,
	memoryBarrierSrcAccessMask = sam,
	memoryBarrierDstAccessMask = dam,
	memoryBarrierOldLayout = olyt,
	memoryBarrierNewLayout = nlyt,
	memoryBarrierSrcQueueFamilyIndex = sqfi,
	memoryBarrierDstQueueFamilyIndex = dqfi,
	memoryBarrierImage = BindedNew img,
	memoryBarrierSubresourceRange = srr } = M.MemoryBarrier {
	M.memoryBarrierNext = mnxt,
	M.memoryBarrierSrcAccessMask = sam,
	M.memoryBarrierDstAccessMask = dam,
	M.memoryBarrierOldLayout = olyt,
	M.memoryBarrierNewLayout = nlyt,
	M.memoryBarrierSrcQueueFamilyIndex = sqfi,
	M.memoryBarrierDstQueueFamilyIndex = dqfi,
	M.memoryBarrierImage = img,
	M.memoryBarrierSubresourceRange = srr }

type family FirstOfFives (tpl :: [(i, j, k, l, m)]) :: [i] where
	FirstOfFives '[] = '[]
	FirstOfFives ('(x, y, z, w, v) ': xyzwvs) = x ': FirstOfFives xyzwvs

class MemoryBarrierListToMiddle
	(nsismnmfmts :: [(Type, Type, Type, Symbol, T.Format)])  where
	memoryBarrierListToMiddle ::
		HeteroVarList (V5 MemoryBarrier) nsismnmfmts ->
		HeteroVarList M.MemoryBarrier (FirstOfFives nsismnmfmts)

instance MemoryBarrierListToMiddle '[] where
	memoryBarrierListToMiddle HVNil = HVNil

instance (Pointable n, MemoryBarrierListToMiddle nsismnmfmts) =>
	MemoryBarrierListToMiddle ('(n, si, sm, nm, fmt) ': nsismnmfmts) where
	memoryBarrierListToMiddle (V5 mb :...: mbs) =
		memoryBarrierToMiddle mb :...: memoryBarrierListToMiddle mbs
