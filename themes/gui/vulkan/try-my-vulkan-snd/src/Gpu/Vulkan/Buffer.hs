{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Buffer where

import GHC.TypeLits
import Foreign.Storable
import Foreign.Pointable
import Control.Exception
import Data.Kind
import Data.Kind.Object
import Data.HeteroList
import Data.Word

import Gpu.Vulkan.Enum hiding (ObjectType)
import Gpu.Vulkan.Buffer.Enum

import qualified Gpu.Vulkan.Core as C
import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Device.Middle as Device.M
import qualified Gpu.Vulkan.Device.Memory.Buffer as Device.Memory
import qualified Gpu.Vulkan.Device.Memory.Buffer.Types as Device.Memory
import qualified Gpu.Vulkan.Memory.Middle as Memory.M
import qualified Gpu.Vulkan.Buffer.Middle as M
import qualified Gpu.Vulkan.Buffer.Core as C
import qualified Gpu.Vulkan.QueueFamily.EnumManual as QueueFamily
import qualified Gpu.Vulkan.Image.Middle as Image.M

data B s (nm :: Symbol) (objs :: [Object]) = B (HeteroVarList ObjectLength objs) C.B

deriving instance Show (HeteroVarList ObjectLength objs) => Show (B s nm objs)

data Binded (sm :: Type) (sb :: Type) (nm :: Symbol) (objs :: [Object]) = Binded (HeteroVarList ObjectLength objs) C.B

deriving instance Show (HeteroVarList ObjectLength objs) => Show (Binded sm sb nm objs)

data CreateInfo n objs = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoLengths :: HeteroVarList ObjectLength objs,
	createInfoUsage :: UsageFlags,
	createInfoSharingMode :: SharingMode,
	createInfoQueueFamilyIndices :: [QueueFamily.Index] }

deriving instance (Show n, Show (HeteroVarList ObjectLength objs)) =>
	Show (CreateInfo n objs)

createInfoToMiddle :: WholeSize objs =>
	CreateInfo n objs -> M.CreateInfo n
createInfoToMiddle CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoLengths = lns,
	createInfoUsage = usg,
	createInfoSharingMode = smd,
	createInfoQueueFamilyIndices = qfis } = M.CreateInfo {
	M.createInfoNext = mnxt,
	M.createInfoFlags = flgs,
	M.createInfoSize = fromIntegral $ wholeSize 0 lns,
	M.createInfoUsage = usg,
	M.createInfoSharingMode = smd,
	M.createInfoQueueFamilyIndices = qfis }

create :: (Pointable n, WholeSize objs, Pointable c, Pointable d) =>
	Device.D ds -> CreateInfo n objs ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	(forall s . B s nm objs -> IO a) -> IO a
create (Device.D dvc) ci macc macd f = bracket
	(M.create dvc (createInfoToMiddle ci) macc)
	(\b -> M.destroy dvc b macd)
	(f . B (createInfoLengths ci) . (\(M.B b) -> b))

getMemoryRequirements :: Device.D sd -> B sb nm objs -> IO Memory.M.Requirements
getMemoryRequirements (Device.D dvc) (B _ b) = M.getMemoryRequirements dvc (M.B b)

getMemoryRequirements' :: Device.D sd -> BB sbobjs -> IO Memory.M.Requirements
getMemoryRequirements' dvc (V3 b) = getMemoryRequirements dvc b

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
	Device.D sd -> HeteroVarList BB sbobjss ->
	Device.Memory.AllocateInfo n ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	(forall s . Device.Memory.M s (SbobjssToObjss sbobjss) -> IO a) -> IO a
allocate dvc@(Device.D mdvc) bs ai macc macd f = bracket
	do	mai <- allocateInfoToMiddle dvc bs ai
		Memory.M.allocate mdvc mai macc
	(\mem -> Memory.M.free mdvc mem macd)
	\(Device.M.Memory mem) -> do
		forms <- bsToForms dvc bs
		f $ Device.Memory.M forms mem

type BB = V3 B
type Bnd sm = V3 (Binded sm)

type family SbobjssToSb (sbobjss :: [(Type, [Object])]) where
	SbobjssToSb '[] = '[]
	SbobjssToSb ('(sb, objs) ': sbobjss) = sb ': SbobjssToSb sbobjss

type family SbobjssToObjss (sbobjss :: [(Type, Symbol, [Object])]) where
	SbobjssToObjss '[] = '[]
	SbobjssToObjss ('(sb, nm, objs) ': sbobjss) = objs ': SbobjssToObjss sbobjss

allocateBind :: (Pointable n, Pointable c, Pointable d) =>
	Device.D sd -> HeteroVarList BB sbobjss ->
	Device.Memory.AllocateInfo n ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) -> (
		forall sm . HeteroVarList (Bnd sm) sbobjss ->
			Device.Memory.M sm (SbobjssToObjss sbobjss) -> IO a ) -> IO a
allocateBind dvc bs ai macc macd f = allocate dvc bs ai macc macd \mem -> do
	bs' <- bindBuffersToMemory dvc bs mem 0
	f bs' mem

bindBuffersToMemory ::
	Device.D sd -> HeteroVarList BB sbobjss -> Device.Memory.M sm objss' ->
	Int -> IO (HeteroVarList (Bnd sm) sbobjss)
bindBuffersToMemory _ HVNil _ _ = pure HVNil
bindBuffersToMemory dvc (V3 b :...: bs) mem i = (:...:)
	<$> (V3 <$> bindMemory dvc b mem i) <*> bindBuffersToMemory dvc bs mem (i + 1)

bindMemory ::
	Device.D sd -> B sb nm objs -> Device.Memory.M sm objss -> Int ->
	IO (Binded sm sb nm objs)
bindMemory (Device.D dvc) (B lns b) (Device.Memory.M fms mem) i = do
	M.bindMemory dvc (M.B b) (Device.M.Memory mem) . fst $ indexForms fms i
	pure $ Binded lns b

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
zipToForms ((ost, sz) : ostszs) (V3 (B lns _) :...: bs) =
	Device.Memory.Form ost sz lns :...: zipToForms ostszs bs
zipToForms _ _ = error "bad"

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

class OffsetList v (vs :: [Object]) where
	offsetList :: HeteroVarList ObjectLength vs -> Int -> Device.M.Size

adjust :: Int -> Int -> Int
adjust algn ost = ((ost - 1) `div` algn + 1) * algn

instance Storable v => OffsetList v ('List v ': vs) where
	offsetList _ = fromIntegral . adjust (alignment @v undefined)

instance {-# OVERLAPPABLE #-} (
	SizeAlignment v', OffsetList v vs ) => OffsetList v (v' ': vs) where
	offsetList (objlen :...: objlens) ost =
		offsetList @v @vs objlens (ost + objectSize objlen)

sampleObjLens :: HeteroVarList ObjectLength
	['List Bool, 'Atom Char, 'Atom Int, 'List Double, 'List Char]
sampleObjLens =
	ObjectLengthList 3 :...:
	ObjectLengthAtom :...:
	ObjectLengthAtom :...:
	ObjectLengthList 5 :...:
	ObjectLengthList 3 :...: HVNil

data IndexedList sm sb nm v =
	forall vs . OffsetList v vs => IndexedList (Binded sm sb nm vs)

indexedListToOffset :: forall sm sb nm v a . IndexedList sm sb nm v ->
	(forall vs . (Binded sm sb nm vs, Device.M.Size) -> a) -> a
indexedListToOffset (IndexedList b@(Binded lns _)) f = f (b, offsetList @v lns 0)

indexedListToMiddle :: IndexedList sm sb nm v -> (M.B, Device.M.Size)
indexedListToMiddle il = indexedListToOffset il \(Binded _ b, sz) -> (M.B b, sz)

indexedListToMiddles ::
	HeteroVarList (V4 IndexedList) smsbvs -> [(M.B, Device.M.Size)]
indexedListToMiddles HVNil = []
indexedListToMiddles (V4 il :...: ils) =
	indexedListToMiddle il : indexedListToMiddles ils

class CopyPrefix (area :: [Object]) (src :: [Object]) (dst :: [Object]) where
	copyCheckLengthPrefix ::
		HeteroVarList ObjectLength src ->
		HeteroVarList ObjectLength dst -> Bool
	copySizePrefix :: Word64 -> HeteroVarList ObjectLength src -> Word64

instance CopyPrefix '[] src dst where
	copyCheckLengthPrefix _ _ = True
	copySizePrefix sz _ = sz

instance (
	SizeAlignment a,
	CopyPrefix as ss ds ) =>
	CopyPrefix (a ': as) (a ': ss) (a ': ds) where
	copyCheckLengthPrefix (s :...: ss) (d :...: ds) =
		s == d && copyCheckLengthPrefix @as ss ds
	copySizePrefix sz (ln :...: lns) = copySizePrefix @as @ss @ds
		(((sz - 1) `div` algn + 1) * algn + fromIntegral (objectSize ln))
		lns
		where algn = fromIntegral $ objectAlignment @a

class CopyInfo (area :: [Object]) (src :: [Object]) (dst :: [Object]) where
	copyCheckLength ::
		HeteroVarList ObjectLength src ->
		HeteroVarList ObjectLength dst -> Bool
	copySrcOffset :: Word64 -> HeteroVarList ObjectLength src -> Word64
	copyDstOffset :: Word64 -> HeteroVarList ObjectLength dst -> Word64
	copySize :: HeteroVarList ObjectLength src -> Word64

type OT o = Data.Kind.Object.ObjectType o

instance (
	Storable (Data.Kind.Object.ObjectType a),
	CopyPrefix (a ': as) (a ': ss) (a ': ds) ) => CopyInfo (a ': as) (a ': ss) (a ': ds) where
	copyCheckLength = copyCheckLengthPrefix @(a ': as) @(a ': ss) @(a ': ds)
	copySrcOffset ost _ = ((ost - 1) `div` algn + 1) * algn
		where algn = fromIntegral $ alignment @(OT a) undefined
	copyDstOffset ost _ = ((ost - 1) `div` algn + 1) * algn
		where algn = fromIntegral $ alignment @(OT a) undefined
	copySize = copySizePrefix @(a ': as) @(a ': ss) @(a ': ds) 0

instance {-# OVERLAPPABLE #-}
	(SizeAlignment d, CopyInfo (a ': as) (a ': ss) ds) =>
	CopyInfo (a ': as) (a ': ss) (d ': ds) where
	copyCheckLength ss (_ :...: ds) =
		copyCheckLength @(a ': as) @(a ': ss) @ds ss ds
	copySrcOffset ost lns = copySrcOffset @(a ': as) @(a ': ss) @ds ost lns
	copyDstOffset ost (ln :...: lns) = copyDstOffset @(a ': as) @(a ': ss)
		(((ost - 1) `div` algn + 1) * algn + fromIntegral (objectSize ln))
		lns
		where algn = fromIntegral $ objectAlignment @d
	copySize = copySize @(a ': as) @(a ': ss) @ds

instance {-# OVERLAPPABLE #-}
	(SizeAlignment s,
	CopyInfo as ss ds) =>
	CopyInfo as (s ': ss) ds where
	copyCheckLength (_ :...: ss) ds = copyCheckLength @as ss ds
	copySrcOffset ost (ln :...: lns) = copySrcOffset @as @ss @ds
		(((ost - 1) `div` algn + 1) * algn + fromIntegral (objectSize ln))
		lns
		where algn = fromIntegral (objectAlignment @s)
	copyDstOffset ost lns = copyDstOffset @as @ss ost lns
	copySize (_ :...: lns) = copySize @as @ss @ds lns

makeCopy :: forall (as :: [Object]) ss ds . CopyInfo as ss ds =>
	HeteroVarList ObjectLength ss -> HeteroVarList ObjectLength ds -> C.Copy
makeCopy src dst
	| copyCheckLength @as src dst = C.Copy {
		C.copySrcOffset = copySrcOffset @as @ss @ds 0 src,
		C.copyDstOffset = copyDstOffset @as @ss @ds 0 dst,
		C.copySize = copySize @as @ss @ds src }
	| otherwise = error "List lengths are different"

class MakeCopies (ass :: [[Object]]) (ss :: [Object]) (ds :: [Object]) where
	makeCopies ::
		HeteroVarList ObjectLength ss ->
		HeteroVarList ObjectLength ds -> [C.Copy]

instance MakeCopies '[] ss ds where makeCopies _ _ = []

instance (CopyInfo as ss ds, MakeCopies ass ss ds) =>
	MakeCopies (as ': ass) ss ds where
	makeCopies src dst = makeCopy @as src dst : makeCopies @ass src dst

class OffsetSize (v :: Object) (vs :: [Object]) where
	offsetSize :: HeteroVarList ObjectLength vs ->
		Device.M.Size -> (Device.M.Size, Device.M.Size)
	objectLength :: HeteroVarList ObjectLength vs -> ObjectLength v

instance SizeAlignment v => OffsetSize v (v ': vs) where
	offsetSize (ln :...: _) ost = (
		((ost - 1) `div` algn + 1) * algn,
		fromIntegral $ objectSize ln )
		where algn = fromIntegral $ objectAlignment @v
	objectLength (ln :...: _) = ln

instance {-# OVERLAPPABLE #-}
	(SizeAlignment v, SizeAlignment v', OffsetSize v vs ) =>
	OffsetSize v (v' ': vs) where
	offsetSize (ln :...: lns) ost = offsetSize @v lns
		$ ((ost - 1) `div` algn + 1) * algn + sz
		where
		algn = fromIntegral $ objectAlignment @v
		sz = fromIntegral $ objectSize ln
	objectLength (_ :...: lns) = objectLength @v lns

data MemoryBarrier n sm sb nm obj = forall objs . OffsetSize obj objs =>
	MemoryBarrier {
		memoryBarrierNext :: Maybe n,
		memoryBarrierSrcAccessMask :: AccessFlags,
		memoryBarrierDstAccessMask :: AccessFlags,
		memoryBarrierSrcQueueFamilyIndex :: QueueFamily.Index,
		memoryBarrierDstQueueFamilyIndex :: QueueFamily.Index,
		memoryBarrierBuffer :: Binded sm sb nm objs }

memoryBarrierToMiddle :: forall n sm sb nm obj .
	MemoryBarrier n sm sb nm obj -> M.MemoryBarrier n
memoryBarrierToMiddle MemoryBarrier {
	memoryBarrierNext = mnxt,
	memoryBarrierSrcAccessMask = sam,
	memoryBarrierDstAccessMask = dam,
	memoryBarrierSrcQueueFamilyIndex = sqfi,
	memoryBarrierDstQueueFamilyIndex = dqfi,
	memoryBarrierBuffer = Binded lns b :: Binded sm sb nm objs } =
	M.MemoryBarrier {
		M.memoryBarrierNext = mnxt,
		M.memoryBarrierSrcAccessMask = sam,
		M.memoryBarrierDstAccessMask = dam,
		M.memoryBarrierSrcQueueFamilyIndex = sqfi,
		M.memoryBarrierDstQueueFamilyIndex = dqfi,
		M.memoryBarrierBuffer = M.B b,
		M.memoryBarrierOffset = ost,
		M.memoryBarrierSize = sz }
	where (ost, sz) = offsetSize @obj lns 0

class MemoryBarrierListToMiddle nsmsbnmobjs where
	memoryBarrierListToMiddle ::
		HeteroVarList (V5 MemoryBarrier) nsmsbnmobjs ->
		HeteroVarList M.MemoryBarrier (FirstOfFives nsmsbnmobjs)

type family FirstOfFives (tpl :: [(i, j, k, l, m)]) :: [i] where
	FirstOfFives '[] = '[]
	FirstOfFives ('(x, y, z, w, v) ': xyzwvs) = x ': FirstOfFives xyzwvs

instance MemoryBarrierListToMiddle '[] where
	memoryBarrierListToMiddle HVNil = HVNil

instance (Pointable n, MemoryBarrierListToMiddle nsmsbnmobjs) =>
	MemoryBarrierListToMiddle ('(n, sm, sb, nm, obj) ': nsmsbnmobjs) where
	memoryBarrierListToMiddle (V5 mb :...: mbs) =
		memoryBarrierToMiddle mb :...: memoryBarrierListToMiddle mbs

data ImageCopy img = ImageCopy {
	imageCopyImageSubresource :: Image.M.SubresourceLayers,
	imageCopyImageOffset :: C.Offset3d,
	imageCopyImageExtent :: C.Extent3d }
	deriving Show

imageCopyToMiddle :: forall img sm sb nm objs .
	OffsetSize ('ObjImage img) objs =>
	Binded sm sb nm objs -> ImageCopy img -> M.ImageCopy
imageCopyToMiddle (Binded lns _) ImageCopy {
	imageCopyImageSubresource = isr,
	imageCopyImageOffset = iost,
	imageCopyImageExtent = iext } = M.ImageCopy {
	M.imageCopyBufferOffset = ost,
	M.imageCopyBufferRowLength = fromIntegral r,
	M.imageCopyBufferImageHeight = fromIntegral h,
	M.imageCopyImageSubresource = isr,
	M.imageCopyImageOffset = iost,
	M.imageCopyImageExtent = iext }
	where
	(ost, _) = offsetSize @('ObjImage img) lns 0
	ObjectLengthImage r _w h _d = objectLength @('ObjImage img) lns
