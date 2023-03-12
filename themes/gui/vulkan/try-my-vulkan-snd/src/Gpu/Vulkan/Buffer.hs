{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Buffer where

import GHC.TypeLits
import Foreign.Storable.PeekPoke
import Control.Exception
import Data.Kind
import Gpu.Vulkan.Object qualified as VObj
import Data.Proxy
import Data.TypeLevel.Uncurry
import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:**))
import Data.Word

import Gpu.Vulkan.Enum hiding (ObjectType)
import Gpu.Vulkan.Buffer.Enum

import qualified Gpu.Vulkan.Middle as C
import qualified Gpu.Vulkan.TypeEnum as T
import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Device.Middle as Device.M
import qualified Gpu.Vulkan.Memory.Middle as Memory.M
import qualified Gpu.Vulkan.Buffer.Middle as M
import qualified Gpu.Vulkan.Buffer.Middle as C
import qualified Gpu.Vulkan.QueueFamily.Middle as QueueFamily
import qualified Gpu.Vulkan.Image.Middle as Image.M

data B s (nm :: Symbol) (objs :: [VObj.Object]) = B (HeteroParList.PL VObj.ObjectLength objs) M.B

deriving instance Show (HeteroParList.PL VObj.ObjectLength objs) => Show (B s nm objs)

data Binded (sm :: Type) (sb :: Type) (nm :: Symbol) (objs :: [VObj.Object]) = Binded (HeteroParList.PL VObj.ObjectLength objs) M.B

deriving instance Show (HeteroParList.PL VObj.ObjectLength objs) => Show (Binded sm sb nm objs)

deriving instance Eq (HeteroParList.PL VObj.ObjectLength objs) => Eq (Binded sm sb nm objs)

data CreateInfo n objs = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoLengths :: HeteroParList.PL VObj.ObjectLength objs,
	createInfoUsage :: UsageFlags,
	createInfoSharingMode :: SharingMode,
	createInfoQueueFamilyIndices :: [QueueFamily.Index] }

deriving instance (Show n, Show (HeteroParList.PL VObj.ObjectLength objs)) =>
	Show (CreateInfo n objs)

createInfoToMiddle :: VObj.WholeSize objs =>
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
	M.createInfoSize = fromIntegral $ VObj.wholeSize 0 lns,
	M.createInfoUsage = usg,
	M.createInfoSharingMode = smd,
	M.createInfoQueueFamilyIndices = qfis }

create :: (WithPoked n, VObj.WholeSize objs, WithPoked c, WithPoked d) =>
	Device.D ds -> CreateInfo n objs ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	(forall s . B s nm objs -> IO a) -> IO a
create (Device.D dvc) ci macc macd f = bracket
	(M.create dvc (createInfoToMiddle ci) macc)
	(\b -> M.destroy dvc b macd)
	(f . B (createInfoLengths ci))

getMemoryRequirements :: Device.D sd -> B sb nm objs -> IO Memory.M.Requirements
getMemoryRequirements (Device.D dvc) (B _ b) = M.getMemoryRequirements dvc b

class OffsetList v (vs :: [VObj.Object]) where
	offsetList :: HeteroParList.PL VObj.ObjectLength vs -> Int -> Device.M.Size

adjust :: Int -> Int -> Int
adjust algn ost = ((ost - 1) `div` algn + 1) * algn

instance (KnownNat algn, WithPoked v, Sizable v) =>
	OffsetList v (VObj.List algn v _nm ': vs) where
	offsetList _ = fromIntegral . adjust (
		fromIntegral (natVal (Proxy :: Proxy algn)) `lcm`
		alignment' @v )

instance {-# OVERLAPPABLE #-} (
	VObj.SizeAlignment v', OffsetList v vs ) => OffsetList v (v' ': vs) where
	offsetList (objlen :** objlens) ost =
		offsetList @v @vs objlens (ost + VObj.objectSize objlen)

{-
sampleObjLens :: HeteroParList.PL NObj.ObjectLength
	['NObj.List 256 Bool "", 'NObj.Atom 256 Char 'Nothing, 'NObj.Atom 256 Int 'Nothing, 'NObj.List 256 Double "", 'NObj.List 256 Char ""]
sampleObjLens =
	NObj.ObjectLengthList 3 :**
	NObj.ObjectLengthAtom :**
	NObj.ObjectLengthAtom :**
	NObj.ObjectLengthList 5 :**
	NObj.ObjectLengthList 3 :** HeteroParList.Nil
	-}

data IndexedList sm sb nm v =
	forall vs . OffsetList v vs => IndexedList (Binded sm sb nm vs)

indexedListToOffset :: forall sm sb nm v a . IndexedList sm sb nm v ->
	(forall vs . (Binded sm sb nm vs, Device.M.Size) -> a) -> a
indexedListToOffset (IndexedList b@(Binded lns _)) f = f (b, offsetList @v lns 0)

indexedListToMiddle :: IndexedList sm sb nm v -> (M.B, Device.M.Size)
indexedListToMiddle il = indexedListToOffset il \(Binded _ b, sz) -> (b, sz)

indexedListToMiddles ::
	HeteroParList.PL (U4 IndexedList) smsbvs -> [(M.B, Device.M.Size)]
indexedListToMiddles HeteroParList.Nil = []
indexedListToMiddles (U4 il :** ils) =
	indexedListToMiddle il : indexedListToMiddles ils

class CopyPrefix (area :: [VObj.Object]) (src :: [VObj.Object]) (dst :: [VObj.Object]) where
	copyCheckLengthPrefix ::
		HeteroParList.PL VObj.ObjectLength src ->
		HeteroParList.PL VObj.ObjectLength dst -> Bool
	copySizePrefix :: Word64 -> HeteroParList.PL VObj.ObjectLength src -> Word64

instance CopyPrefix '[] src dst where
	copyCheckLengthPrefix _ _ = True
	copySizePrefix sz _ = sz

instance (
	VObj.SizeAlignment a,
	CopyPrefix as ss ds ) =>
	CopyPrefix (a ': as) (a ': ss) (a ': ds) where
	copyCheckLengthPrefix (s :** ss) (d :** ds) =
		s == d && copyCheckLengthPrefix @as ss ds
	copySizePrefix sz (ln :** lns) = copySizePrefix @as @ss @ds
		(((sz - 1) `div` algn + 1) * algn + fromIntegral (VObj.objectSize ln))
		lns
		where algn = fromIntegral $ VObj.objectAlignment @a

class CopyInfo (area :: [VObj.Object]) (src :: [VObj.Object]) (dst :: [VObj.Object]) where
	copyCheckLength ::
		HeteroParList.PL VObj.ObjectLength src ->
		HeteroParList.PL VObj.ObjectLength dst -> Bool
	copySrcOffset :: Word64 -> HeteroParList.PL VObj.ObjectLength src -> Word64
	copyDstOffset :: Word64 -> HeteroParList.PL VObj.ObjectLength dst -> Word64
	copySize :: HeteroParList.PL VObj.ObjectLength src -> Word64

type OT o = VObj.ObjectType o

instance (
	WithPoked (VObj.ObjectType a),
	Sizable (VObj.ObjectType a),
	CopyPrefix (a ': as) (a ': ss) (a ': ds) ) => CopyInfo (a ': as) (a ': ss) (a ': ds) where
	copyCheckLength = copyCheckLengthPrefix @(a ': as) @(a ': ss) @(a ': ds)
	copySrcOffset ost _ = ((ost - 1) `div` algn + 1) * algn
		where algn = fromIntegral $ alignment' @(OT a)
	copyDstOffset ost _ = ((ost - 1) `div` algn + 1) * algn
		where algn = fromIntegral $ alignment' @(OT a)
	copySize = copySizePrefix @(a ': as) @(a ': ss) @(a ': ds) 0

instance {-# OVERLAPPABLE #-}
	(VObj.SizeAlignment d, CopyInfo (a ': as) (a ': ss) ds) =>
	CopyInfo (a ': as) (a ': ss) (d ': ds) where
	copyCheckLength ss (_ :** ds) =
		copyCheckLength @(a ': as) @(a ': ss) @ds ss ds
	copySrcOffset ost lns = copySrcOffset @(a ': as) @(a ': ss) @ds ost lns
	copyDstOffset ost (ln :** lns) = copyDstOffset @(a ': as) @(a ': ss)
		(((ost - 1) `div` algn + 1) * algn + fromIntegral (VObj.objectSize ln))
		lns
		where algn = fromIntegral $ VObj.objectAlignment @d
	copySize = copySize @(a ': as) @(a ': ss) @ds

instance {-# OVERLAPPABLE #-}
	(VObj.SizeAlignment s,
	CopyInfo as ss ds) =>
	CopyInfo as (s ': ss) ds where
	copyCheckLength (_ :** ss) ds = copyCheckLength @as ss ds
	copySrcOffset ost (ln :** lns) = copySrcOffset @as @ss @ds
		(((ost - 1) `div` algn + 1) * algn + fromIntegral (VObj.objectSize ln))
		lns
		where algn = fromIntegral (VObj.objectAlignment @s)
	copyDstOffset ost lns = copyDstOffset @as @ss ost lns
	copySize (_ :** lns) = copySize @as @ss @ds lns

makeCopy :: forall (as :: [VObj.Object]) ss ds . CopyInfo as ss ds =>
	HeteroParList.PL VObj.ObjectLength ss -> HeteroParList.PL VObj.ObjectLength ds -> C.Copy
makeCopy src dst
	| copyCheckLength @as src dst = C.Copy {
		C.copySrcOffset = copySrcOffset @as @ss @ds 0 src,
		C.copyDstOffset = copyDstOffset @as @ss @ds 0 dst,
		C.copySize = copySize @as @ss @ds src }
	| otherwise = error "List lengths are different"

class MakeCopies (ass :: [[VObj.Object]]) (ss :: [VObj.Object]) (ds :: [VObj.Object]) where
	makeCopies ::
		HeteroParList.PL VObj.ObjectLength ss ->
		HeteroParList.PL VObj.ObjectLength ds -> [C.Copy]

instance MakeCopies '[] ss ds where makeCopies _ _ = []

instance (CopyInfo as ss ds, MakeCopies ass ss ds) =>
	MakeCopies (as ': ass) ss ds where
	makeCopies src dst = makeCopy @as src dst : makeCopies @ass src dst

class OffsetSize (v :: VObj.Object) (vs :: [VObj.Object]) where
	offsetSize :: HeteroParList.PL VObj.ObjectLength vs ->
		Device.M.Size -> (Device.M.Size, Device.M.Size)
	objectLength :: HeteroParList.PL VObj.ObjectLength vs -> VObj.ObjectLength v

instance VObj.SizeAlignment v => OffsetSize v (v ': vs) where
	offsetSize (ln :** _) ost = (
		((ost - 1) `div` algn + 1) * algn,
		fromIntegral $ VObj.objectSize ln )
		where algn = fromIntegral $ VObj.objectAlignment @v
	objectLength (ln :** _) = ln

instance {-# OVERLAPPABLE #-}
	(VObj.SizeAlignment v, VObj.SizeAlignment v', OffsetSize v vs ) =>
	OffsetSize v (v' ': vs) where
	offsetSize (ln :** lns) ost = offsetSize @v lns
		$ ((ost - 1) `div` algn + 1) * algn + sz
		where
		algn = fromIntegral $ VObj.objectAlignment @v
		sz = fromIntegral $ VObj.objectSize ln
	objectLength (_ :** lns) = objectLength @v lns

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
		M.memoryBarrierBuffer = b,
		M.memoryBarrierOffset = ost,
		M.memoryBarrierSize = sz }
	where (ost, sz) = offsetSize @obj lns 0

class MemoryBarrierListToMiddle nsmsbnmobjs where
	memoryBarrierListToMiddle ::
		HeteroParList.PL (U5 MemoryBarrier) nsmsbnmobjs ->
		HeteroParList.PL M.MemoryBarrier (FirstOfFives nsmsbnmobjs)

type family FirstOfFives (tpl :: [(i, j, k, l, m)]) :: [i] where
	FirstOfFives '[] = '[]
	FirstOfFives ('(x, y, z, w, v) ': xyzwvs) = x ': FirstOfFives xyzwvs

instance MemoryBarrierListToMiddle '[] where
	memoryBarrierListToMiddle HeteroParList.Nil = HeteroParList.Nil

instance (WithPoked n, MemoryBarrierListToMiddle nsmsbnmobjs) =>
	MemoryBarrierListToMiddle ('(n, sm, sb, nm, obj) ': nsmsbnmobjs) where
	memoryBarrierListToMiddle (U5 mb :** mbs) =
		memoryBarrierToMiddle mb :** memoryBarrierListToMiddle mbs

data ImageCopy img inm = ImageCopy {
	imageCopyImageSubresource :: Image.M.SubresourceLayers,
	imageCopyImageOffset :: C.Offset3d,
	imageCopyImageExtent :: C.Extent3d }
	deriving Show

imageCopyToMiddle :: forall algn img (inm :: Symbol) sm sb nm objs .
	OffsetSize (VObj.ObjImage algn img inm) objs =>
	Binded sm sb nm objs -> ImageCopy img inm -> M.ImageCopy
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
	(ost, _) = offsetSize @(VObj.ObjImage algn img inm) lns 0
	VObj.ObjectLengthImage r _w h _d = objectLength @(VObj.ObjImage algn img inm) lns

type family ImageFormat img :: T.Format
