{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Old.Gpu.Vulkan.Device.Memory.Buffer.Bind (allocateBind) where

import GHC.TypeLits
import Foreign.Pointable
import Control.Exception
import Data.Kind
import Data.Kind.Object
import Data.HeteroList
import Data.IORef

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Device.Middle.Internal as Device.M
import qualified Gpu.Vulkan.Memory.AllocateInfo as Device.Memory
import qualified Gpu.Vulkan.Buffer.Middle as M

import qualified Old.Gpu.Vulkan.Device.Memory.Buffer as Device.Memory
import qualified Old.Gpu.Vulkan.Device.Memory.Buffer.Types as Device.Memory

import qualified Gpu.Vulkan.Buffer as Buffer
-- import qualified Old.Gpu.Vulkan.Device.Memory.Buffer as Old.Memory.Buffer
import qualified Gpu.Vulkan.Memory.Middle as Memory.M

allocateBind :: (Pointable n, Pointable c, Pointable d) =>
	Device.D sd -> HeteroVarList BB sbobjss ->
	Device.Memory.AllocateInfo n ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) -> (
		forall sm . HeteroVarList (Bnd sm) sbobjss ->
			Device.Memory.M sm (SbobjssToObjss sbobjss) -> IO a ) -> IO a
allocateBind dvc bs ai macc macd f = allocate dvc bs ai macc macd \mem -> do
	bs' <- bindBuffersToMemory dvc bs mem 0
	f bs' mem

allocate :: (Pointable n, Pointable c, Pointable d) =>
	Device.D sd -> HeteroVarList BB sbobjss ->
	Device.Memory.AllocateInfo n ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	(forall s . Device.Memory.M s (SbobjssToObjss sbobjss) -> IO a) -> IO a
allocate dvc@(Device.D mdvc) bs ai macc macd f = bracket
	do	mai <- allocateInfoToMiddle dvc bs ai
		Memory.M.allocate mdvc mai macc
	(\mem -> Memory.M.free mdvc mem macd)
	\(Memory.M.M mem) -> do
		forms <- bsToForms dvc bs
		m <- readIORef mem
		f $ Device.Memory.M forms m

bindBuffersToMemory ::
	Device.D sd -> HeteroVarList BB sbobjss -> Device.Memory.M sm objss' ->
	Int -> IO (HeteroVarList (Bnd sm) sbobjss)
bindBuffersToMemory _ HVNil _ _ = pure HVNil
bindBuffersToMemory dvc (V3 b :...: bs) mem i = (:...:)
	<$> (V3 <$> bindMemory dvc b mem i) <*> bindBuffersToMemory dvc bs mem (i + 1)

bindMemory ::
	Device.D sd -> Buffer.B sb nm objs -> Device.Memory.M sm objss -> Int ->
	IO (Buffer.Binded sm sb nm objs)
bindMemory (Device.D dvc) (Buffer.B lns b) (Device.Memory.M fms m) i = do
	mem <- newIORef m
	M.bindMemory dvc b (Memory.M.M mem) . fst $ indexForms fms i
	pure $ Buffer.Binded lns b

indexForms :: HeteroVarList Device.Memory.Form objss -> Int ->
	(Device.M.Size, Device.M.Size)
indexForms (Device.Memory.Form ost sz _ :...: _) 0 = (ost, sz)
indexForms (_ :...: fms) i = indexForms fms (i - 1)
indexForms _ _ = error "bad"

bsToForms :: Device.D sd -> HeteroVarList BB sbobjss ->
	IO (HeteroVarList Device.Memory.Form (SbobjssToObjss sbobjss))
bsToForms dvc bs = do
	reqss <- heteroVarListToListM (getMemoryRequirements' dvc) bs
	pure $ zipToForms
		(memoryRequirementsListToOffsets 0 reqss) bs

zipToForms ::
	[(Device.M.Size, Device.M.Size)] -> HeteroVarList BB sbobjss ->
	HeteroVarList Device.Memory.Form (SbobjssToObjss sbobjss)
zipToForms [] HVNil = HVNil
zipToForms ((ost, sz) : ostszs) (V3 (Buffer.B lns _) :...: bs) =
	Device.Memory.Form ost sz lns :...: zipToForms ostszs bs
zipToForms _ _ = error "bad"


allocateInfoToMiddle ::
	Device.D sd -> HeteroVarList BB sbobjss -> Device.Memory.AllocateInfo n ->
	IO (Memory.M.AllocateInfo n)
allocateInfoToMiddle dvc bs Device.Memory.AllocateInfo {
	Device.Memory.allocateInfoNext = mnxt,
	Device.Memory.allocateInfoMemoryTypeIndex = mti } = do
	reqss <- heteroVarListToListM (getMemoryRequirements' dvc) bs
	pure Memory.M.AllocateInfo {
		Memory.M.allocateInfoNext = mnxt,
		Memory.M.allocateInfoAllocationSize =
			memoryRequirementsListToSize 0 reqss,
		Memory.M.allocateInfoMemoryTypeIndex = mti }

getMemoryRequirements' :: Device.D sd -> BB sbobjs -> IO Memory.M.Requirements
getMemoryRequirements' dvc (V3 b) = Buffer.getMemoryRequirements dvc b

memoryRequirementsListToOffsets ::
	Device.M.Size -> [Memory.M.Requirements] ->
	[(Device.M.Size, Device.M.Size)]
memoryRequirementsListToOffsets _ [] = []
memoryRequirementsListToOffsets sz0 (reqs : reqss) =
	(ost, sz) : memoryRequirementsListToOffsets (ost + sz) reqss
	where
	ost = ((sz0 - 1) `div` algn + 1) * algn
	sz = Memory.M.requirementsSize reqs
	algn = Memory.M.requirementsAlignment reqs

memoryRequirementsListToSize ::
	Device.M.Size -> [Memory.M.Requirements] -> Device.M.Size
memoryRequirementsListToSize sz0 [] = sz0
memoryRequirementsListToSize sz0 (reqs : reqss) =
	memoryRequirementsListToSize
		(((sz0 - 1) `div` algn + 1) * algn + sz) reqss
	where
	sz = Memory.M.requirementsSize reqs
	algn = Memory.M.requirementsAlignment reqs

type family SbobjssToSb (sbobjss :: [(Type, [Object])]) where
	SbobjssToSb '[] = '[]
	SbobjssToSb ('(sb, objs) ': sbobjss) = sb ': SbobjssToSb sbobjss

type family SbobjssToObjss (sbobjss :: [(Type, Symbol, [Object])]) where
	SbobjssToObjss '[] = '[]
	SbobjssToObjss ('(sb, nm, objs) ': sbobjss) = objs ': SbobjssToObjss sbobjss

type Bnd sm = V3 (Buffer.Binded sm)
type BB = V3 Buffer.B
