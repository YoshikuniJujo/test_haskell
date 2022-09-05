{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Device.Memory.ImageBuffer where

import Foreign.Storable
import Foreign.Pointable
import Control.Exception hiding (try)
import Data.Kind
import Data.Kind.Object hiding (Offset(..))
import Data.HeteroList

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Device.Middle as Device.M
import qualified Gpu.Vulkan.Device.Core as Device.C
import qualified Gpu.Vulkan.Image.Type as Image
import qualified Gpu.Vulkan.Image.Middle as Image.M
import qualified Gpu.Vulkan.Buffer as Buffer
import qualified Gpu.Vulkan.Buffer.Middle as Buffer.M
import qualified Gpu.Vulkan.Device.Memory.ImageBuffer.Kind as K
import qualified Gpu.Vulkan.Device.Memory.Buffer as Device.Memory.Buffer
import qualified Gpu.Vulkan.Memory.Middle as Memory.M

data M s (sibfoss :: [(Type, K.ImageBuffer)]) =
	M (HeteroVarList (V2 ImageBuffer) sibfoss) Device.C.Memory

deriving instance Show (HeteroVarList (V2 ImageBuffer) sibfoss) =>
	Show (M s sibfoss)

data ImageBuffer sib (ib :: K.ImageBuffer) where
	Image :: Image.INew si nm fmt -> ImageBuffer si ('K.Image nm fmt)
	Buffer :: Buffer.B sb objs -> ImageBuffer sb ('K.Buffer objs)

deriving instance Show (Image.INew sib nm fmt) =>
	Show (ImageBuffer sib ('K.Image nm fmt))

deriving instance Show (HeteroVarList ObjectLength objs) =>
	Show (ImageBuffer sib ('K.Buffer objs))

data ImageBufferBinded sm sib (ib :: K.ImageBuffer) where
	ImageBinded :: Image.BindedNew si sm nm fmt ->
		ImageBufferBinded sm si ('K.Image nm fmt)
	BufferBinded :: Buffer.Binded sb sm objs ->
		ImageBufferBinded sm sb ('K.Buffer objs)

getMemoryRequirements ::
	Device.D sd -> ImageBuffer sib fos -> IO Memory.M.Requirements
getMemoryRequirements (Device.D dvc) (Buffer (Buffer.B _ b)) =
	Buffer.M.getMemoryRequirements dvc (Buffer.M.B b)
getMemoryRequirements (Device.D dvc) (Image (Image.INew i)) =
	Image.M.getMemoryRequirements dvc i

getMemoryRequirements' ::
	Device.D sd -> V2 ImageBuffer sibfos -> IO Memory.M.Requirements
getMemoryRequirements' dvc (V2 bi) = getMemoryRequirements dvc bi

getMemoryRequirementsList :: Device.D sd ->
	HeteroVarList (V2 ImageBuffer) sibfoss -> IO [Memory.M.Requirements]
getMemoryRequirementsList dvc bis =
	heteroVarListToListM (getMemoryRequirements' dvc) bis

allocateInfoToMiddle ::
	Device.D sd -> HeteroVarList (V2 ImageBuffer) sibfoss ->
	Device.Memory.Buffer.AllocateInfo n -> IO (Memory.M.AllocateInfo n)
allocateInfoToMiddle dvc ibs Device.Memory.Buffer.AllocateInfo {
	Device.Memory.Buffer.allocateInfoNext = mnxt,
	Device.Memory.Buffer.allocateInfoMemoryTypeIndex = mti } = do
	reqss <- getMemoryRequirementsList dvc ibs
	pure Memory.M.AllocateInfo {
		Memory.M.allocateInfoNext = mnxt,
		Memory.M.allocateInfoAllocationSize =
			memoryRequirementsListToSize 0 reqss,
		Memory.M.allocateInfoMemoryTypeIndex = mti }

memoryRequirementsListToSize ::
	Device.M.Size -> [Memory.M.Requirements] -> Device.M.Size
memoryRequirementsListToSize sz0 [] = sz0
memoryRequirementsListToSize sz0 (reqs : reqss) =
	memoryRequirementsListToSize
		(((sz0 - 1) `div` algn + 1) * algn + sz) reqss
	where
	sz = Memory.M.requirementsSize reqs
	algn = Memory.M.requirementsAlignment reqs

allocate :: (Pointable n, Pointable c, Pointable d) =>
	Device.D sd ->
	HeteroVarList (V2 ImageBuffer) sibfoss ->
	Device.Memory.Buffer.AllocateInfo n ->
	Maybe (AllocationCallbacks.A c) ->
	Maybe (AllocationCallbacks.A d) ->
	(forall s . M s sibfoss -> IO a) -> IO a
allocate dvc@(Device.D mdvc) bs ai macc macd f = bracket
	do	mai <- allocateInfoToMiddle dvc bs ai
		Memory.M.allocate mdvc mai macc
	(\mem -> Memory.M.free mdvc mem macd)
	\(Device.M.Memory mem) -> f $ M bs mem

allocateBind :: (
	Pointable n, Pointable c, Pointable d, BindAll sibfoss sibfoss ) =>
	Device.D sd ->
	HeteroVarList (V2 ImageBuffer) sibfoss ->
	Device.Memory.Buffer.AllocateInfo n ->
	Maybe (AllocationCallbacks.A c) ->
	Maybe (AllocationCallbacks.A d) ->
	(forall s .
		HeteroVarList (V2 (ImageBufferBinded s)) sibfoss ->
		M s sibfoss -> IO a) -> IO a
allocateBind dvc bs ai macc macd f = allocate dvc bs ai macc macd \m -> do
	bnds <- bindAll dvc bs m
	f bnds m

class BindAll sibfoss sibfoss' where
	bindAll :: Device.D sd -> HeteroVarList (V2 ImageBuffer) sibfoss ->
		M sm sibfoss' ->
		IO (HeteroVarList (V2 (ImageBufferBinded sm)) sibfoss)

instance BindAll '[] sibfoss' where bindAll _ _ _ = pure HVNil

instance (Offset si ('K.Image nm fmt) sibfoss', BindAll fibfoss sibfoss') =>
	BindAll ('(si, ('K.Image nm fmt)) ': fibfoss) sibfoss' where
	bindAll dvc (V2 (Image img) :...: ibs) m = (:...:)
		<$> (V2 . ImageBinded <$> bindImage dvc img m)
		<*> bindAll dvc ibs m

instance (Offset sb ('K.Buffer objs) sibfoss', BindAll fibfoss sibfoss') =>
	BindAll ('(sb, ('K.Buffer objs)) ': fibfoss) sibfoss' where
	bindAll dvc (V2 (Buffer bf) :...: ibs) m = (:...:)
		<$> (V2 . BufferBinded <$> bindBuffer dvc bf m)
		<*> bindAll dvc ibs m

bindImage :: forall sd si nm fmt sm sibfoss . Offset si ('K.Image nm fmt) sibfoss =>
	Device.D sd -> Image.INew si nm fmt -> M sm sibfoss ->
	IO (Image.BindedNew si sm nm fmt)
bindImage dvc@(Device.D mdvc) (Image.INew i) m@(M _ mm) = do
	ost <- offset @si @('K.Image nm fmt) dvc m 0
	Image.M.bindMemory mdvc i (Device.M.MemoryImage mm) ost
	pure (Image.BindedNew i)

bindBuffer :: forall sd sb objs sm sibfoss . Offset sb ('K.Buffer objs) sibfoss =>
	Device.D sd -> Buffer.B sb objs -> M sm sibfoss ->
	IO (Buffer.Binded sb sm objs)
bindBuffer dvc@(Device.D mdvc) (Buffer.B lns b) m@(M _ mm) = do
	ost <- offset @sb @('K.Buffer objs) dvc m 0
	Buffer.M.bindMemory mdvc (Buffer.M.B b) (Device.M.Memory mm) ost
	pure (Buffer.Binded lns b)

class Offset
	sib (ib :: K.ImageBuffer) (sibfoss :: [(Type, K.ImageBuffer)]) where
	offset :: Device.D sd -> M sm sibfoss -> Device.M.Size -> IO Device.M.Size

instance Offset sib ib ('(sib, ib) ': sibfoss) where
	offset dvc (M (ib :...: _ibs) _m) ost = do
		reqs <- getMemoryRequirements' dvc ib
		let	algn = Memory.M.requirementsAlignment reqs
		pure $ ((ost - 1) `div` algn + 1) * algn

instance {-# OVERLAPPABLE #-} Offset sib ib sibfoss =>
	Offset sib ib ('(sib', ib') ': sibfoss) where
	offset dvc (M (ib :...: ibs) m) ost = do
		reqs <- getMemoryRequirements' dvc ib
		let	sz = Memory.M.requirementsSize reqs
			algn = Memory.M.requirementsAlignment reqs
		offset @sib @ib dvc (M ibs m)
			$ ((ost - 1) `div` algn + 1) * algn + sz

class Try s sibfoss where
	try :: Device.D sd -> M sm sibfoss -> IO (Device.M.Size, Device.M.Size)

instance Try sib ('(sib, ib) ': sibfoss) where
	try dvc (M (ib :...: _) _) = do
		reqs <- getMemoryRequirements' dvc ib
		let	sz = Memory.M.requirementsSize reqs
			algn = Memory.M.requirementsAlignment reqs
		pure (sz, algn)

instance {-# OVERLAPPABLE #-} Try sib sibfoss =>
	Try sib ('(sib', ib) ': sibfoss) where
	try dvc (M (_ :...: ibs) m) = try @sib dvc (M ibs m)
