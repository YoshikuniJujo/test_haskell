{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Buffer.Internal (

	-- * CREATE

	create, B, CreateInfo(..),

	-- ** Buffer Group

	Group, group, create', unsafeDestroy, lookup,

	-- * BINDED

	getMemoryRequirements, Binded, lengthBinded,
	IndexedForList(..), indexedListToMiddles, indexedListToMiddle,

	-- * COPY

	MakeCopies(..), CopyInfo,

	-- * IMAGE COPY

	ImageCopy(..), ImageCopyListToMiddle(..),

	-- * MEMORY BARRIER

	MemoryBarrier(..), MemoryBarrierListToMiddle(..)
	
	) where

import Prelude hiding (lookup)
import GHC.TypeLits
import Foreign.Storable.PeekPoke
import Control.Exception
import Gpu.Vulkan.Object qualified as VObj
import Data.Kind
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Tuple.MapIndex qualified as TMapIndex
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.HeteroParList qualified as HeteroParList
import Data.HeteroParList (pattern (:**))
import Data.Word

import Gpu.Vulkan.Enum hiding (ObjectType)
import Gpu.Vulkan.Buffer.Enum

import qualified Gpu.Vulkan.Middle as C
import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.AllocationCallbacks.Type as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Device.Middle as Device.M
import qualified Gpu.Vulkan.Memory as Memory
import qualified Gpu.Vulkan.Buffer.Middle as M
import qualified Gpu.Vulkan.Buffer.Middle as C
import qualified Gpu.Vulkan.QueueFamily as QueueFamily
import qualified Gpu.Vulkan.Image as Image.M

import Gpu.Vulkan.Buffer.Type

import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Data.Map qualified as Map

data CreateInfo mn objs = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: CreateFlags,
	createInfoLengths :: HeteroParList.PL VObj.Length objs,
	createInfoUsage :: UsageFlags,
	createInfoSharingMode :: SharingMode,
	createInfoQueueFamilyIndices :: [QueueFamily.Index] }

deriving instance
	(Show (TMaybe.M n), Show (HeteroParList.PL VObj.Length objs)) =>
	Show (CreateInfo n objs)

createInfoToMiddle :: VObj.SizeAlignmentList objs =>
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
	M.createInfoSize = VObj.wholeSize lns,
	M.createInfoUsage = usg,
	M.createInfoSharingMode = smd,
	M.createInfoQueueFamilyIndices = qfis }

create :: (
	WithPoked (TMaybe.M mn),
	VObj.SizeAlignmentList objs, AllocationCallbacks.ToMiddle ma ) =>
	Device.D sd -> CreateInfo mn objs ->
	TPMaybe.M (U2 AllocationCallbacks.A) ma ->
	(forall sb . B sb nm objs -> IO a) -> IO a
create (Device.D dvc) ci (AllocationCallbacks.toMiddle -> mac) f = bracket
	(M.create dvc (createInfoToMiddle ci) mac) (\b -> M.destroy dvc b mac)
	(f . B (createInfoLengths ci))

data Group sd ma s k nm objs = Group (Device.D sd)
	(TPMaybe.M (U2 AllocationCallbacks.A) ma)
	TSem (TVar (Map.Map k (B s nm objs)))

group :: AllocationCallbacks.ToMiddle md =>
	Device.D sd -> TPMaybe.M (U2 AllocationCallbacks.A) md ->
	(forall s . Group sd md s k nm objs -> IO a) -> IO a
group dvc@(Device.D mdvc) mac@(AllocationCallbacks.toMiddle -> mmac) f = do
	(sem, m) <- atomically $ (,) <$> newTSem 1 <*> newTVar Map.empty
	rtn <- f $ Group dvc mac sem m
	((\(B _ mb) -> M.destroy mdvc mb mmac) `mapM_`)
		=<< atomically (readTVar m)
	pure rtn

create' :: (
	Ord k, WithPoked (TMaybe.M mn),
	VObj.SizeAlignmentList objs, AllocationCallbacks.ToMiddle ma ) =>
	Group sd ma sg k nm objs -> k -> CreateInfo mn objs ->
	IO (Either String (B sg nm objs))
create' (Group (Device.D dvc)
	(AllocationCallbacks.toMiddle -> mac) sem bs) k ci = do
	ok <- atomically do
		mx <- (Map.lookup k) <$> readTVar bs
		case mx of
			Nothing -> waitTSem sem >> pure True
			Just _ -> pure False
	if ok
	then do	b <- M.create dvc (createInfoToMiddle ci) mac
		let b' = B (createInfoLengths ci) b
		atomically $ modifyTVar bs (Map.insert k b') >> signalTSem sem
		pure $ Right b'
	else pure . Left $ "Gpu.Vulkan.Buffer.create': The key already exist"

unsafeDestroy :: (Ord k, AllocationCallbacks.ToMiddle ma) =>
	Group sd ma sg k nm objs -> k -> IO (Either String ())
unsafeDestroy (Group (Device.D mdvc)
	(AllocationCallbacks.toMiddle -> ma) sem bs) k = do
	mb <- atomically do
		mx <- Map.lookup k <$> readTVar bs
		case mx of
			Nothing -> pure Nothing
			Just _ -> waitTSem sem >> pure mx
	case mb of
		Nothing -> pure $ Left "Gpu.Vulkan.Buffer.unsafeDestroy: No such key"
		Just (B _ b) -> do
			M.destroy mdvc b ma
			atomically do
				modifyTVar bs (Map.delete k)
				signalTSem sem
				pure $ Right ()

lookup :: Ord k => Group sd md sg k nm objs -> k -> IO (Maybe (B sg nm objs))
lookup (Group _ _ _sem bs) k = atomically $ Map.lookup k <$> readTVar bs

getMemoryRequirements :: Device.D sd -> B sb nm objs -> IO Memory.Requirements
getMemoryRequirements (Device.D dvc) (B _ b) = M.getMemoryRequirements dvc b

data IndexedForList sm sb nm t onm = forall objs .
	VObj.OffsetOfList t onm objs => IndexedForList (Binded sm sb nm objs)

indexedListToOffset :: forall sm sb nm v onm a . IndexedForList sm sb nm v onm ->
	(forall vs . (Binded sm sb nm vs, Device.M.Size) -> a) -> a
indexedListToOffset (IndexedForList b@(Binded lns _)) f =
	f (b, fst $ VObj.offsetOfList @v @onm lns)

indexedListToMiddle :: IndexedForList sm sb nm v onm -> (M.B, Device.M.Size)
indexedListToMiddle il = indexedListToOffset il \(Binded _ b, sz) -> (b, sz)

indexedListToMiddles ::
	HeteroParList.PL (U5 IndexedForList) smsbvs -> [(M.B, Device.M.Size)]
indexedListToMiddles HeteroParList.Nil = []
indexedListToMiddles (U5 il :** ils) =
	indexedListToMiddle il : indexedListToMiddles ils

class CopyPrefix (area :: [VObj.O]) (src :: [VObj.O]) (dst :: [VObj.O]) where
	copyCheckLengthPrefix ::
		HeteroParList.PL VObj.Length src ->
		HeteroParList.PL VObj.Length dst -> Bool
	copySizePrefix :: Word64 -> HeteroParList.PL VObj.Length src -> Word64

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
		(((sz - 1) `div` algn + 1) * algn + fromIntegral (VObj.size ln))
		lns
		where algn = fromIntegral $ VObj.alignment @a

class CopyInfo (area :: [VObj.O]) (is :: Nat) (id :: Nat) (src :: [VObj.O]) (dst :: [VObj.O]) where
	copyCheckLength ::
		HeteroParList.PL VObj.Length src ->
		HeteroParList.PL VObj.Length dst -> Bool
	copySrcOffset :: Word64 -> HeteroParList.PL VObj.Length src -> Word64
	copyDstOffset :: Word64 -> HeteroParList.PL VObj.Length dst -> Word64
	copySize :: HeteroParList.PL VObj.Length src -> Word64

type OT o = VObj.TypeOf o

instance (
	Sizable (VObj.TypeOf a),
	CopyPrefix (a ': as) (a ': ss) (a ': ds) ) => CopyInfo (a ': as) 0 0 (a ': ss) (a ': ds) where
	copyCheckLength = copyCheckLengthPrefix @(a ': as) @(a ': ss) @(a ': ds)
	copySrcOffset ost _ = ((ost - 1) `div` algn + 1) * algn
		where algn = fromIntegral $ alignment' @(OT a)
	copyDstOffset ost _ = ((ost - 1) `div` algn + 1) * algn
		where algn = fromIntegral $ alignment' @(OT a)
	copySize = copySizePrefix @(a ': as) @(a ': ss) @(a ': ds) 0

instance {-# OVERLAPPABLE #-}
	(VObj.SizeAlignment a, CopyInfo (a ': as) 0 (id - 1) (a ': ss) ds) =>
	CopyInfo (a ': as) 0 id (a ': ss) (a ': ds) where
	copyCheckLength ss (_ :** ds) =
		copyCheckLength @(a ': as) @0 @(id - 1) @(a ': ss) @ds ss ds
	copySrcOffset ost lns = copySrcOffset @(a ': as) @0 @(id - 1) @(a ': ss) @ds ost lns
	copyDstOffset ost (ln :** lns) = copyDstOffset @(a ': as) @0 @(id - 1) @(a ': ss)
		(((ost - 1) `div` algn + 1) * algn + fromIntegral (VObj.size ln))
		lns
		where algn = fromIntegral $ VObj.alignment @a
	copySize = copySize @(a ': as) @0 @(id - 1) @(a ': ss) @ds

instance {-# OVERLAPPABLE #-}
	(VObj.SizeAlignment d, CopyInfo (a ': as) 0 id (a ': ss) ds) =>
	CopyInfo (a ': as) 0 id (a ': ss) (d ': ds) where
	copyCheckLength ss (_ :** ds) =
		copyCheckLength @(a ': as) @0 @id @(a ': ss) @ds ss ds
	copySrcOffset ost lns = copySrcOffset @(a ': as) @0 @id @(a ': ss) @ds ost lns
	copyDstOffset ost (ln :** lns) = copyDstOffset @(a ': as) @0 @id @(a ': ss)
		(((ost - 1) `div` algn + 1) * algn + fromIntegral (VObj.size ln))
		lns
		where algn = fromIntegral $ VObj.alignment @d
	copySize = copySize @(a ': as) @0 @id @(a ': ss) @ds

instance {-# OVERLAPPABLE #-}
	(VObj.SizeAlignment a, CopyInfo (a ': as) (is - 1) id ss ds) =>
	CopyInfo (a ': as) is id (a ': ss) ds where
	copyCheckLength (_ :** ss) ds = copyCheckLength @(a ': as) @(is - 1) @id @ss @ds ss ds
	copySrcOffset ost (ln :** lns) = copySrcOffset @(a ': as) @(is - 1) @id @ss @ds
		(((ost - 1) `div` algn + 1) * algn + fromIntegral (VObj.size ln))
		lns
		where algn = fromIntegral $ VObj.alignment @a
	copyDstOffset ost lns = copyDstOffset @(a ': as) @(is - 1) @id @ss ost lns
	copySize (_ :** lns) = copySize @(a ': as) @(is - 1) @id @ss @ds lns

instance {-# OVERLAPPABLE #-}
	(VObj.SizeAlignment s,
	CopyInfo as is id ss ds) =>
	CopyInfo as is id (s ': ss) ds where
	copyCheckLength (_ :** ss) ds = copyCheckLength @as @is @id ss ds
	copySrcOffset ost (ln :** lns) = copySrcOffset @as @is @id @ss @ds
		(((ost - 1) `div` algn + 1) * algn + fromIntegral (VObj.size ln))
		lns
		where algn = fromIntegral (VObj.alignment @s)
	copyDstOffset ost lns = copyDstOffset @as @is @id @ss ost lns
	copySize (_ :** lns) = copySize @as @is @id @ss @ds lns

makeCopy :: forall (as :: [VObj.O]) is id ss ds . CopyInfo as is id ss ds =>
	HeteroParList.PL VObj.Length ss -> HeteroParList.PL VObj.Length ds -> C.Copy
makeCopy src dst
	| copyCheckLength @as @is @id src dst = C.Copy {
		C.copySrcOffset = copySrcOffset @as @is @id @ss @ds 0 src,
		C.copyDstOffset = copyDstOffset @as @is @id @ss @ds 0 dst,
		C.copySize = copySize @as @is @id @ss @ds src }
	| otherwise = error "List lengths are different"

class MakeCopies (cpss :: [([VObj.O], Nat, Nat)]) (ss :: [VObj.O]) (ds :: [VObj.O]) where
	makeCopies ::
		HeteroParList.PL VObj.Length ss ->
		HeteroParList.PL VObj.Length ds -> [C.Copy]

instance MakeCopies '[] ss ds where makeCopies _ _ = []

instance (CopyInfo as is id ss ds, MakeCopies ass ss ds) =>
	MakeCopies ('(as, is, id) ': ass) ss ds where
	makeCopies src dst = makeCopy @as @is @id src dst : makeCopies @ass src dst

{-
offsetSize :: forall v vs . (
	VObj.OffsetRange v vs, VObj.SizeAlignment v,
	VObj.LengthOf v vs ) =>
	HeteroParList.PL VObj.Length vs ->
	Device.M.Size -> (Device.M.Size, Device.M.Size)
offsetSize lns _ = (VObj.offsetNew @v lns, sizeNew @v lns)

sizeNew :: forall v vs . (
	VObj.SizeAlignment v, VObj.LengthOf v vs ) =>
	HeteroParList.PL VObj.Length vs -> Device.M.Size
sizeNew = fromIntegral . VObj.size . VObj.objectLengthOf @v
-}

data MemoryBarrier mn sm sb nm obj = forall objs . (
	VObj.OffsetRange obj objs 0, VObj.LengthOf obj objs ) =>
	MemoryBarrier {
		memoryBarrierNext :: TMaybe.M mn,
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
	where (ost, sz) = VObj.offsetRange @obj @_ @0 0 lns

class MemoryBarrierListToMiddle nsmsbnmobjs where
	memoryBarrierListToMiddle ::
		HeteroParList.PL (U5 MemoryBarrier) nsmsbnmobjs ->
		HeteroParList.PL M.MemoryBarrier (TMapIndex.M0_5 nsmsbnmobjs)

instance MemoryBarrierListToMiddle '[] where
	memoryBarrierListToMiddle HeteroParList.Nil = HeteroParList.Nil

instance MemoryBarrierListToMiddle nsmsbnmobjs =>
	MemoryBarrierListToMiddle ('(mn, sm, sb, nm, obj) ': nsmsbnmobjs) where
	memoryBarrierListToMiddle (U5 mb :** mbs) =
		memoryBarrierToMiddle mb :** memoryBarrierListToMiddle mbs

data ImageCopy img inm = ImageCopy {
	imageCopyImageSubresource :: Image.M.SubresourceLayers,
	imageCopyImageOffset :: C.Offset3d,
	imageCopyImageExtent :: C.Extent3d }
	deriving Show

class ImageCopyListToMiddle algn objs (img :: Type) (inms :: [Symbol]) where
	imageCopyListToMiddle ::
		Binded sm sb nm objs ->
		HeteroParList.PL (ImageCopy img) inms ->
		[M.ImageCopy]

instance ImageCopyListToMiddle algn objs img '[] where
	imageCopyListToMiddle _ HeteroParList.Nil = []

instance (
	VObj.OffsetRange (VObj.Image algn img nm) objs 0,
	VObj.LengthOf (VObj.Image algn img nm) objs,
	ImageCopyListToMiddle algn objs img nms ) =>
	ImageCopyListToMiddle algn objs img (nm ': nms) where
	imageCopyListToMiddle bf (ic :** ics) =
		imageCopyToMiddle @algn @_ @nm bf (ic :: ImageCopy img nm) :
		imageCopyListToMiddle @algn bf ics

imageCopyToMiddle :: forall algn img inm sm sb nm obj objs . (
	obj ~ VObj.Image algn img inm,
	VObj.OffsetRange obj objs 0, VObj.LengthOf obj objs ) =>
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
	(ost, _) = VObj.offsetRange @(VObj.Image algn img inm) @_ @0 0 lns
	VObj.LengthImage r _w h _d = VObj.lengthOf @obj lns
