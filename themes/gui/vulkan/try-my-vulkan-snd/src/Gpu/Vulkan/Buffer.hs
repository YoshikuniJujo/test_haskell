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
import Foreign.Storable
import Control.Exception
import Data.Kind
import Data.Kind.Object hiding (objectLength)
import Data.Proxy
import Data.TypeLevel.Uncurry
import qualified Data.HeteroParList as HeteroParList
import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:*), pattern (:**))
import Data.Word

import Gpu.Vulkan.Enum hiding (ObjectType)
import Gpu.Vulkan.Buffer.Enum

import qualified Gpu.Vulkan.Core as C
import qualified Gpu.Vulkan.TypeEnum as T
import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Device.Middle.Internal as Device.M
import qualified Gpu.Vulkan.Memory.Middle as Memory.M
import qualified Gpu.Vulkan.Buffer.Middle as M
import qualified Gpu.Vulkan.Buffer.Core as C
import qualified Gpu.Vulkan.QueueFamily.EnumManual as QueueFamily
import qualified Gpu.Vulkan.Image.Middle as Image.M

data B s (nm :: Symbol) (objs :: [Object]) = B (HeteroParList.PL ObjectLength objs) M.B

deriving instance Show (HeteroParList.PL ObjectLength objs) => Show (B s nm objs)

data Binded (sm :: Type) (sb :: Type) (nm :: Symbol) (objs :: [Object]) = Binded (HeteroParList.PL ObjectLength objs) M.B

deriving instance Show (HeteroParList.PL ObjectLength objs) => Show (Binded sm sb nm objs)

deriving instance Eq (HeteroParList.PL ObjectLength objs) => Eq (Binded sm sb nm objs)

data CreateInfo n objs = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoLengths :: HeteroParList.PL ObjectLength objs,
	createInfoUsage :: UsageFlags,
	createInfoSharingMode :: SharingMode,
	createInfoQueueFamilyIndices :: [QueueFamily.Index] }

deriving instance (Show n, Show (HeteroParList.PL ObjectLength objs)) =>
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

create :: (Storable n, WholeSize objs, Storable c, Storable d) =>
	Device.D ds -> CreateInfo n objs ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	(forall s . B s nm objs -> IO a) -> IO a
create (Device.D dvc) ci macc macd f = bracket
	(M.create dvc (createInfoToMiddle ci) macc)
	(\b -> M.destroy dvc b macd)
	(f . B (createInfoLengths ci))

getMemoryRequirements :: Device.D sd -> B sb nm objs -> IO Memory.M.Requirements
getMemoryRequirements (Device.D dvc) (B _ b) = M.getMemoryRequirements dvc b

class OffsetList v (vs :: [Object]) where
	offsetList :: HeteroParList.PL ObjectLength vs -> Int -> Device.M.Size

adjust :: Int -> Int -> Int
adjust algn ost = ((ost - 1) `div` algn + 1) * algn

instance (KnownNat algn, Storable v) =>
	OffsetList v ('List algn v _nm ': vs) where
	offsetList _ = fromIntegral . adjust (
		fromIntegral (natVal (Proxy :: Proxy algn)) `lcm`
		alignment @v undefined )

instance {-# OVERLAPPABLE #-} (
	SizeAlignment v', OffsetList v vs ) => OffsetList v (v' ': vs) where
	offsetList (objlen :** objlens) ost =
		offsetList @v @vs objlens (ost + objectSize objlen)

sampleObjLens :: HeteroParList.PL ObjectLength
	['List 256 Bool "", 'Atom 256 Char 'Nothing, 'Atom 256 Int 'Nothing, 'List 256 Double "", 'List 256 Char ""]
sampleObjLens =
	ObjectLengthList 3 :**
	ObjectLengthAtom :**
	ObjectLengthAtom :**
	ObjectLengthList 5 :**
	ObjectLengthList 3 :** HeteroParList.Nil

data IndexedList sm sb nm v =
	forall vs . OffsetList v vs => IndexedList (Binded sm sb nm vs)

indexedListToOffset :: forall sm sb nm v a . IndexedList sm sb nm v ->
	(forall vs . (Binded sm sb nm vs, Device.M.Size) -> a) -> a
indexedListToOffset (IndexedList b@(Binded lns _)) f = f (b, offsetList @v lns 0)

indexedListToMiddle :: IndexedList sm sb nm v -> (M.B, Device.M.Size)
indexedListToMiddle il = indexedListToOffset il \(Binded _ b, sz) -> (b, sz)

indexedListToMiddles ::
	HeteroParList.PL (V4 IndexedList) smsbvs -> [(M.B, Device.M.Size)]
indexedListToMiddles HeteroParList.Nil = []
indexedListToMiddles (V4 il :** ils) =
	indexedListToMiddle il : indexedListToMiddles ils

class CopyPrefix (area :: [Object]) (src :: [Object]) (dst :: [Object]) where
	copyCheckLengthPrefix ::
		HeteroParList.PL ObjectLength src ->
		HeteroParList.PL ObjectLength dst -> Bool
	copySizePrefix :: Word64 -> HeteroParList.PL ObjectLength src -> Word64

instance CopyPrefix '[] src dst where
	copyCheckLengthPrefix _ _ = True
	copySizePrefix sz _ = sz

instance (
	SizeAlignment a,
	CopyPrefix as ss ds ) =>
	CopyPrefix (a ': as) (a ': ss) (a ': ds) where
	copyCheckLengthPrefix (s :** ss) (d :** ds) =
		s == d && copyCheckLengthPrefix @as ss ds
	copySizePrefix sz (ln :** lns) = copySizePrefix @as @ss @ds
		(((sz - 1) `div` algn + 1) * algn + fromIntegral (objectSize ln))
		lns
		where algn = fromIntegral $ objectAlignment @a

class CopyInfo (area :: [Object]) (src :: [Object]) (dst :: [Object]) where
	copyCheckLength ::
		HeteroParList.PL ObjectLength src ->
		HeteroParList.PL ObjectLength dst -> Bool
	copySrcOffset :: Word64 -> HeteroParList.PL ObjectLength src -> Word64
	copyDstOffset :: Word64 -> HeteroParList.PL ObjectLength dst -> Word64
	copySize :: HeteroParList.PL ObjectLength src -> Word64

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
	copyCheckLength ss (_ :** ds) =
		copyCheckLength @(a ': as) @(a ': ss) @ds ss ds
	copySrcOffset ost lns = copySrcOffset @(a ': as) @(a ': ss) @ds ost lns
	copyDstOffset ost (ln :** lns) = copyDstOffset @(a ': as) @(a ': ss)
		(((ost - 1) `div` algn + 1) * algn + fromIntegral (objectSize ln))
		lns
		where algn = fromIntegral $ objectAlignment @d
	copySize = copySize @(a ': as) @(a ': ss) @ds

instance {-# OVERLAPPABLE #-}
	(SizeAlignment s,
	CopyInfo as ss ds) =>
	CopyInfo as (s ': ss) ds where
	copyCheckLength (_ :** ss) ds = copyCheckLength @as ss ds
	copySrcOffset ost (ln :** lns) = copySrcOffset @as @ss @ds
		(((ost - 1) `div` algn + 1) * algn + fromIntegral (objectSize ln))
		lns
		where algn = fromIntegral (objectAlignment @s)
	copyDstOffset ost lns = copyDstOffset @as @ss ost lns
	copySize (_ :** lns) = copySize @as @ss @ds lns

makeCopy :: forall (as :: [Object]) ss ds . CopyInfo as ss ds =>
	HeteroParList.PL ObjectLength ss -> HeteroParList.PL ObjectLength ds -> C.Copy
makeCopy src dst
	| copyCheckLength @as src dst = C.Copy {
		C.copySrcOffset = copySrcOffset @as @ss @ds 0 src,
		C.copyDstOffset = copyDstOffset @as @ss @ds 0 dst,
		C.copySize = copySize @as @ss @ds src }
	| otherwise = error "List lengths are different"

class MakeCopies (ass :: [[Object]]) (ss :: [Object]) (ds :: [Object]) where
	makeCopies ::
		HeteroParList.PL ObjectLength ss ->
		HeteroParList.PL ObjectLength ds -> [C.Copy]

instance MakeCopies '[] ss ds where makeCopies _ _ = []

instance (CopyInfo as ss ds, MakeCopies ass ss ds) =>
	MakeCopies (as ': ass) ss ds where
	makeCopies src dst = makeCopy @as src dst : makeCopies @ass src dst

class OffsetSize (v :: Object) (vs :: [Object]) where
	offsetSize :: HeteroParList.PL ObjectLength vs ->
		Device.M.Size -> (Device.M.Size, Device.M.Size)
	objectLength :: HeteroParList.PL ObjectLength vs -> ObjectLength v

instance SizeAlignment v => OffsetSize v (v ': vs) where
	offsetSize (ln :** _) ost = (
		((ost - 1) `div` algn + 1) * algn,
		fromIntegral $ objectSize ln )
		where algn = fromIntegral $ objectAlignment @v
	objectLength (ln :** _) = ln

instance {-# OVERLAPPABLE #-}
	(SizeAlignment v, SizeAlignment v', OffsetSize v vs ) =>
	OffsetSize v (v' ': vs) where
	offsetSize (ln :** lns) ost = offsetSize @v lns
		$ ((ost - 1) `div` algn + 1) * algn + sz
		where
		algn = fromIntegral $ objectAlignment @v
		sz = fromIntegral $ objectSize ln
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
		HeteroParList.PL (V5 MemoryBarrier) nsmsbnmobjs ->
		HeteroParList.PL M.MemoryBarrier (FirstOfFives nsmsbnmobjs)

type family FirstOfFives (tpl :: [(i, j, k, l, m)]) :: [i] where
	FirstOfFives '[] = '[]
	FirstOfFives ('(x, y, z, w, v) ': xyzwvs) = x ': FirstOfFives xyzwvs

instance MemoryBarrierListToMiddle '[] where
	memoryBarrierListToMiddle HeteroParList.Nil = HeteroParList.Nil

instance (Storable n, MemoryBarrierListToMiddle nsmsbnmobjs) =>
	MemoryBarrierListToMiddle ('(n, sm, sb, nm, obj) ': nsmsbnmobjs) where
	memoryBarrierListToMiddle (V5 mb :** mbs) =
		memoryBarrierToMiddle mb :** memoryBarrierListToMiddle mbs

data ImageCopy img inm = ImageCopy {
	imageCopyImageSubresource :: Image.M.SubresourceLayers,
	imageCopyImageOffset :: C.Offset3d,
	imageCopyImageExtent :: C.Extent3d }
	deriving Show

imageCopyToMiddle :: forall img (inm :: Symbol) sm sb nm objs .
	OffsetSize ('ObjImage img inm) objs =>
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
	(ost, _) = offsetSize @('ObjImage img inm) lns 0
	ObjectLengthImage r _w h _d = objectLength @('ObjImage img inm) lns

type family ImageFormat img :: T.Format
