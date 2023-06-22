{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Memory (
	M, allocateBind, reallocateBind, read, write,

	ImageBuffer(..), ImageBufferBinded(..),

	OffsetSize'(..), OffsetSizeObject(..), getMemoryRequirementsList,
	offsetSize,
	Alignments,

	M.Requirements(..)
	) where

import Prelude hiding (map, read)
import GHC.TypeLits
import Foreign.Ptr
import Foreign.Storable.PeekPoke
import Control.Exception hiding (try)
import Data.Kind
import Gpu.Vulkan.Object qualified as VObj
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.Tuple.Uncurry
import Data.Maybe
import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:**))
import Data.IORef

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.AllocationCallbacks.Type as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Device.Middle as Device.M
import qualified Gpu.Vulkan.Image.Type as Image
import qualified Gpu.Vulkan.Image.Middle as Image.M
import qualified Gpu.Vulkan.Buffer.Type as Buffer
import qualified Gpu.Vulkan.Buffer.Middle as Buffer.M
import qualified Gpu.Vulkan.Memory.Kind as K
import qualified Gpu.Vulkan.Memory.AllocateInfo as Device.Memory.Buffer
import qualified Gpu.Vulkan.Memory.Middle as Memory.M
import qualified Gpu.Vulkan.Memory.Middle as M

data M s (sibfoss :: [(Type, K.ImageBuffer)]) =
	M (IORef (HeteroParList.PL (U2 ImageBuffer) sibfoss)) M.M

readM'' :: M s sibfoss -> IO (HeteroParList.PL (U2 ImageBuffer) sibfoss, M.M)
readM'' (M ib m) = (, m) <$> readIORef ib

writeMBinded' :: M s sibfoss ->
	HeteroParList.PL (U2 (ImageBufferBinded sm)) sibfoss -> IO ()
writeMBinded' (M rib _r) ibs = writeIORef rib (HeteroParList.map imageBufferFromBinded ibs)

imageBufferFromBinded :: U2 (ImageBufferBinded sm) sibfos -> U2 ImageBuffer sibfos
imageBufferFromBinded (U2 (ImageBinded (Image.Binded i))) = U2 . Image $ Image.INew i
imageBufferFromBinded (U2 (BufferBinded (Buffer.Binded x b))) = U2 . Buffer $ Buffer.B x b

newM2' :: HeteroParList.PL (U2 ImageBuffer) sibfoss -> M.M -> IO (M s sibfoss)
newM2' ibs mm = (`M` mm) <$> newIORef ibs

-- deriving instance Show (HeteroParList.PL (U2 ImageBuffer) sibfoss) =>
--	Show (M s sibfoss)

data ImageBuffer sib (ib :: K.ImageBuffer) where
	Image :: Image.INew si nm fmt -> ImageBuffer si ('K.Image nm fmt)
	Buffer :: Buffer.B sb nm objs -> ImageBuffer sb ('K.Buffer nm objs)

class Alignments (ibs :: [(Type, K.ImageBuffer)]) where
	alignments :: [Maybe Int]

instance Alignments '[] where alignments = []

instance Alignments ibs =>
	Alignments ('(_s, 'K.Image _nm _fmt) ': ibs) where
	alignments = Nothing : alignments @ibs

instance (VObj.SizeAlignment obj, Alignments ibs) =>
	Alignments ('(_s, 'K.Buffer _nm (obj ': _objs)) ': ibs) where
	alignments = Just (VObj.objectAlignment @obj) : alignments @ibs

deriving instance Show (Image.INew sib nm fmt) =>
	Show (ImageBuffer sib ('K.Image nm fmt))

deriving instance Show (HeteroParList.PL VObj.ObjectLength objs) =>
	Show (ImageBuffer sib ('K.Buffer nm objs))

data ImageBufferBinded sm sib (ib :: K.ImageBuffer) where
	ImageBinded :: Image.Binded sm si nm fmt ->
		ImageBufferBinded sm si ('K.Image nm fmt)
	BufferBinded :: Buffer.Binded sm sb nm objs ->
		ImageBufferBinded sm sb ('K.Buffer nm objs)

getMemoryRequirements ::
	Device.D sd -> ImageBuffer sib fos -> IO Memory.M.Requirements
getMemoryRequirements (Device.D dvc) (Buffer (Buffer.B _ b)) =
	Buffer.M.getMemoryRequirements dvc b
getMemoryRequirements (Device.D dvc) (Image (Image.INew i)) =
	Image.M.getMemoryRequirements dvc i

getMemoryRequirementsBinded ::
	Device.D sd -> ImageBufferBinded sm sib fos -> IO Memory.M.Requirements
getMemoryRequirementsBinded (Device.D dvc) (BufferBinded (Buffer.Binded _ b)) =
	Buffer.M.getMemoryRequirements dvc b
getMemoryRequirementsBinded (Device.D dvc) (ImageBinded (Image.Binded i)) =
	Image.M.getMemoryRequirements dvc i

getMemoryRequirements' ::
	Device.D sd -> U2 ImageBuffer sibfos -> IO Memory.M.Requirements
getMemoryRequirements' dvc (U2 bi) = getMemoryRequirements dvc bi

getMemoryRequirementsBinded' ::
	Device.D sd -> U2 (ImageBufferBinded sm) sibfos -> IO Memory.M.Requirements
getMemoryRequirementsBinded' dvc (U2 bi) = getMemoryRequirementsBinded dvc bi

getMemoryRequirementsList :: Device.D sd ->
	HeteroParList.PL (U2 ImageBuffer) sibfoss -> IO [Memory.M.Requirements]
getMemoryRequirementsList dvc bis =
	HeteroParList.toListM (getMemoryRequirements' dvc) bis

getMemoryRequirementsListBinded :: Device.D sd ->
	HeteroParList.PL (U2 (ImageBufferBinded sm)) sibfoss -> IO [Memory.M.Requirements]
getMemoryRequirementsListBinded dvc bis =
	HeteroParList.toListM (getMemoryRequirementsBinded' dvc) bis

allocateInfoToMiddle :: forall sd sibfoss n . Alignments sibfoss =>
	Device.D sd -> HeteroParList.PL (U2 ImageBuffer) sibfoss ->
	Device.Memory.Buffer.AllocateInfo n -> IO (Memory.M.AllocateInfo n)
allocateInfoToMiddle dvc ibs Device.Memory.Buffer.AllocateInfo {
	Device.Memory.Buffer.allocateInfoNext = mnxt,
	Device.Memory.Buffer.allocateInfoMemoryTypeIndex = mti } = do
	reqss <- getMemoryRequirementsList dvc ibs
	pure Memory.M.AllocateInfo {
		Memory.M.allocateInfoNext = mnxt,
		Memory.M.allocateInfoAllocationSize =
			memoryRequirementsListToSize 0
				(alignments @sibfoss) reqss,
		Memory.M.allocateInfoMemoryTypeIndex = mti }

reallocateInfoToMiddle :: forall sd sm sibfoss n . Alignments sibfoss =>
	Device.D sd -> HeteroParList.PL (U2 (ImageBufferBinded sm)) sibfoss ->
	Device.Memory.Buffer.AllocateInfo n -> IO (Memory.M.AllocateInfo n)
reallocateInfoToMiddle dvc ibs Device.Memory.Buffer.AllocateInfo {
	Device.Memory.Buffer.allocateInfoNext = mnxt,
	Device.Memory.Buffer.allocateInfoMemoryTypeIndex = mti } = do
	reqss <- getMemoryRequirementsListBinded dvc ibs
	pure Memory.M.AllocateInfo {
		Memory.M.allocateInfoNext = mnxt,
		Memory.M.allocateInfoAllocationSize =
			memoryRequirementsListToSize 0
				(alignments @sibfoss) reqss,
		Memory.M.allocateInfoMemoryTypeIndex = mti }

memoryRequirementsListToSize ::
	Device.M.Size -> [Maybe Int] -> [Memory.M.Requirements] -> Device.M.Size
memoryRequirementsListToSize sz0 _ [] = sz0
memoryRequirementsListToSize sz0 [] _ = sz0
memoryRequirementsListToSize sz0 (malgn : malgns) (reqs : reqss) =
	memoryRequirementsListToSize
		(((sz0 - 1) `div` algn + 1) * algn + sz) malgns reqss
	where
	sz = Memory.M.requirementsSize reqs
--	algn = Memory.M.requirementsAlignment reqs
	algn = fromIntegral (fromMaybe 1 malgn) `lcm`
		Memory.M.requirementsAlignment reqs

allocate :: (
	WithPoked (TMaybe.M n), Alignments sibfoss,
	AllocationCallbacks.ToMiddle mscc ) =>
	Device.D sd ->
	HeteroParList.PL (U2 ImageBuffer) sibfoss ->
	Device.Memory.Buffer.AllocateInfo n ->
	TPMaybe.M (U2 AllocationCallbacks.A) mscc ->
	(forall s . M s sibfoss -> IO a) -> IO a
allocate dvc@(Device.D mdvc) bs ai
	(AllocationCallbacks.toMiddle -> mac) f = bracket
	do	mai <- allocateInfoToMiddle dvc bs ai
		Memory.M.allocate mdvc mai mac
	(\mem -> Memory.M.free mdvc mem mac)
	\mem -> f =<< newM2' bs mem

reallocate :: (
	WithPoked (TMaybe.M n), Alignments sibfoss,
	AllocationCallbacks.ToMiddle mscc ) =>
	Device.D sd -> HeteroParList.PL (U2 (ImageBufferBinded sm)) sibfoss ->
	Device.Memory.Buffer.AllocateInfo n ->
	TPMaybe.M (U2 AllocationCallbacks.A) mscc -> M sm sibfoss -> IO ()
reallocate dvc@(Device.D mdvc) bs ai
	(AllocationCallbacks.toMiddle -> mac) mem = do
	mai <- reallocateInfoToMiddle dvc bs ai
	(_, oldmem) <- readM'' mem
	Memory.M.reallocate mdvc mai mac oldmem
	writeMBinded' mem bs

reallocateBind :: (
	WithPoked (TMaybe.M n), RebindAll sibfoss sibfoss, Alignments sibfoss,
	AllocationCallbacks.ToMiddle mscc ) =>
	Device.D sd -> HeteroParList.PL (U2 (ImageBufferBinded sm)) sibfoss ->
	Device.Memory.Buffer.AllocateInfo n ->
	TPMaybe.M (U2 AllocationCallbacks.A) mscc -> M sm sibfoss -> IO ()
reallocateBind dvc bs ai macc mem = do
	reallocate dvc bs ai macc mem
	rebindAll dvc bs mem

class RebindAll sibfoss sibfoss' where
	rebindAll :: Device.D sd ->
		HeteroParList.PL (U2 (ImageBufferBinded sm)) sibfoss ->
		M sm sibfoss' -> IO ()

instance RebindAll '[] sibfoss' where rebindAll _ _ _ = pure ()

instance (
	Offset' si ('K.Image nm fmt) sibfoss', RebindAll fibfoss sibfoss' ) =>
	RebindAll ('(si, 'K.Image nm fmt) ': fibfoss) sibfoss' where
		rebindAll dvc (U2 (ImageBinded img) :** ibs) m = do
			rebindImage dvc img m
			rebindAll dvc ibs m

instance (
	Offset' sb ('K.Buffer nm objs) sibfoss', RebindAll sibfoss sibfoss' ) =>
	RebindAll ('(sb, 'K.Buffer nm objs) ': sibfoss) sibfoss' where
	rebindAll dvc (U2 (BufferBinded bf) :** ibs) m = do
		rebindBuffer dvc bf m
		rebindAll dvc ibs m

allocateBind :: (
	WithPoked (TMaybe.M n),
	BindAll sibfoss sibfoss, Alignments sibfoss,
	AllocationCallbacks.ToMiddle mscc ) =>
	Device.D sd ->
	HeteroParList.PL (U2 ImageBuffer) sibfoss ->
	Device.Memory.Buffer.AllocateInfo n ->
	TPMaybe.M (U2 AllocationCallbacks.A) mscc ->
	(forall s .
		HeteroParList.PL (U2 (ImageBufferBinded s)) sibfoss ->
		M s sibfoss -> IO a) -> IO a
allocateBind dvc bs ai macc f = allocate dvc bs ai macc \m -> do
	bnds <- bindAll dvc bs m
	f bnds m

class BindAll sibfoss sibfoss' where
	bindAll :: Device.D sd -> HeteroParList.PL (U2 ImageBuffer) sibfoss ->
		M sm sibfoss' ->
		IO (HeteroParList.PL (U2 (ImageBufferBinded sm)) sibfoss)

instance BindAll '[] sibfoss' where bindAll _ _ _ = pure HeteroParList.Nil

instance (Offset' si ('K.Image nm fmt) sibfoss', BindAll fibfoss sibfoss') =>
	BindAll ('(si, ('K.Image nm fmt)) ': fibfoss) sibfoss' where
	bindAll dvc (U2 (Image img) :** ibs) m = (:**)
		<$> (U2 . ImageBinded <$> bindImage dvc img m)
		<*> bindAll dvc ibs m

instance (Offset' sb ('K.Buffer nm objs) sibfoss', BindAll fibfoss sibfoss') =>
	BindAll ('(sb, ('K.Buffer nm objs)) ': fibfoss) sibfoss' where
	bindAll dvc (U2 (Buffer bf) :** ibs) m = (:**)
		<$> (U2 . BufferBinded <$> bindBuffer dvc bf m)
		<*> bindAll dvc ibs m

bindImage :: forall sd si nm fmt sm sibfoss .
	Offset' si ('K.Image nm fmt) sibfoss =>
	Device.D sd -> Image.INew si nm fmt -> M sm sibfoss ->
	IO (Image.Binded sm si nm fmt)
bindImage dvc@(Device.D mdvc) (Image.INew i) m = do
	(_, mm) <- readM'' m
	ost <- offset @si @('K.Image nm fmt) dvc m 0
	Image.M.bindMemory mdvc i mm ost
	pure (Image.Binded i)

rebindImage :: forall sd si sm nm fmt sibfoss .
	Offset' si ('K.Image nm fmt) sibfoss =>
	Device.D sd -> Image.Binded sm si nm fmt -> M sm sibfoss -> IO ()
rebindImage dvc@(Device.D mdvc) (Image.Binded i) m = do
	(_, mm) <- readM'' m
	ost <- offset @si @('K.Image nm fmt) dvc m 0
	Image.M.bindMemory mdvc i mm ost

bindBuffer :: forall sd sb nm objs sm sibfoss . Offset' sb ('K.Buffer nm objs) sibfoss =>
	Device.D sd -> Buffer.B sb nm objs -> M sm sibfoss ->
	IO (Buffer.Binded sm sb nm objs)
bindBuffer dvc@(Device.D mdvc) (Buffer.B lns b) m = do
	(_, mm) <- readM'' m
	ost <- offset @sb @('K.Buffer nm objs) dvc m 0
	Buffer.M.bindMemory mdvc b mm ost
	pure (Buffer.Binded lns b)

rebindBuffer :: forall sd sb sm nm objs sibfoss .
	Offset' sb ('K.Buffer nm objs) sibfoss =>
	Device.D sd -> Buffer.Binded sm sb nm objs -> M sm sibfoss -> IO ()
rebindBuffer dvc@(Device.D mdvc) (Buffer.Binded _lns b) m = do
	(_, mm) <- readM'' m
	ost <- offset @sb @('K.Buffer nm objs) dvc m 0
	Buffer.M.bindMemory mdvc b mm ost

offset :: forall sib ib sibfoss sd sm . Offset' sib ib sibfoss =>
	Device.D sd -> M sm sibfoss -> Device.M.Size -> IO Device.M.Size
offset dvc m ost = do
	(ibs, _) <- readM'' m
	offset' @sib @ib @sibfoss dvc ibs ost

class Offset'
	sib (ib :: K.ImageBuffer) (sibfoss :: [(Type, K.ImageBuffer)]) where
	offset' :: Device.D sd -> HeteroParList.PL (U2 ImageBuffer) sibfoss ->
		Device.M.Size -> IO Device.M.Size

instance Offset' sib ib ('(sib, ib) ': sibfoss) where
	offset' dvc (ib :** _ibs) ost = do
		reqs <- getMemoryRequirements' dvc ib
		let	algn = Memory.M.requirementsAlignment reqs
		pure $ ((ost - 1) `div` algn + 1) * algn

instance {-# OVERLAPPABLE #-} Offset' sib ib sibfoss =>
	Offset' sib ib ('(sib', ib') ': sibfoss) where
	offset' dvc (ib :** ibs) ost = do
		reqs <- getMemoryRequirements' dvc ib
		let	sz = Memory.M.requirementsSize reqs
			algn = Memory.M.requirementsAlignment reqs
		offset' @sib @ib dvc ibs
			$ ((ost - 1) `div` algn + 1) * algn + sz

offsetSize :: forall nm obj sibfoss sd sm . OffsetSize' nm obj sibfoss =>
	Device.D sd -> M sm sibfoss -> Device.M.Size ->
	IO (Device.M.Size, Device.M.Size)
offsetSize dvc m ost = do
	(ibs, _m) <- readM'' m
	offsetSize' @nm @obj @sibfoss dvc ibs ost

offsetSizeLength :: forall nm obj sibfoss sm . OffsetSize' nm obj sibfoss =>
	M sm sibfoss -> IO (VObj.ObjectLength obj)
offsetSizeLength m = do
	(lns, _m) <- readM'' m
	offsetSizeLength' @nm @obj @sibfoss lns

class OffsetSize' (nm :: Symbol) (obj :: VObj.Object) sibfoss where
	offsetSize' :: Device.D sd -> HeteroParList.PL (U2 ImageBuffer) sibfoss ->
		Device.M.Size -> IO (Device.M.Size, Device.M.Size)
	offsetSizeLength' :: HeteroParList.PL (U2 ImageBuffer) sibfoss -> IO (VObj.ObjectLength obj)

instance OffsetSizeObject obj objs =>
	OffsetSize' nm obj ('(sib, 'K.Buffer nm objs) ': sibfoss) where
	offsetSize' dvc (ib@(U2 (Buffer (Buffer.B lns _))) :** _ibs) ost = do
		reqs <- getMemoryRequirements' dvc ib
		let	algn = Memory.M.requirementsAlignment reqs
		pure $ offsetSizeObject @obj
			(((ost - 1) `div` algn + 1) * algn) lns
	offsetSizeLength' (U2 (Buffer (Buffer.B lns _)) :** _) =
		pure $ offsetSizeObjectLength @obj lns

instance {-# OVERLAPPABLE #-}
	OffsetSize' nm obj sibfoss =>
	OffsetSize' nm obj ('(sib, ib) ': sibfoss) where
	offsetSize' dvc (ib :** ibs) ost = do
		reqs <- getMemoryRequirements' dvc ib
		let	algn = Memory.M.requirementsAlignment reqs
			sz = Memory.M.requirementsSize reqs
		offsetSize' @nm @obj dvc ibs
			$ ((ost - 1) `div` algn + 1) * algn + sz
	offsetSizeLength' (_ :** lns) = offsetSizeLength' @nm @obj lns

class OffsetSizeObject (obj :: VObj.Object) (objs :: [VObj.Object]) where
	offsetSizeObject :: Device.M.Size -> HeteroParList.PL VObj.ObjectLength objs ->
		(Device.M.Size, Device.M.Size)
	offsetSizeObjectLength :: HeteroParList.PL VObj.ObjectLength objs ->
		VObj.ObjectLength obj

instance VObj.SizeAlignment obj => OffsetSizeObject obj (obj ': objs) where
	offsetSizeObject n (ln :** _) = (ost, fromIntegral $ VObj.objectSize' ln)
		where
		ost = ((n - 1) `div` algn + 1) * algn
		algn = fromIntegral (VObj.objectAlignment @obj)
	offsetSizeObjectLength (ln :** _) = ln

instance {-# OVERLAPPABLE #-} (
	VObj.SizeAlignment obj, VObj.SizeAlignment obj',
	OffsetSizeObject obj objs ) =>
	OffsetSizeObject obj (obj' ': objs) where
	offsetSizeObject n (ln :** lns) =
		offsetSizeObject @obj (ost + fromIntegral (VObj.objectSize' ln)) lns
		where
		ost = ((n - 1) `div` algn + 1) * algn
		algn = fromIntegral (VObj.objectAlignment @obj)
	offsetSizeObjectLength (_ :** lns) = offsetSizeObjectLength @obj lns

write :: forall nm obj sd sm sibfoss v .
	(VObj.StoreObject v obj, OffsetSize' nm obj sibfoss) =>
	Device.D sd -> M sm sibfoss -> Memory.M.MapFlags -> v -> IO ()
write dvc mem flgs v = bracket
	(map @nm @obj dvc mem flgs) (const $ unmap dvc mem)
	(\(ptr :: Ptr (VObj.ObjectType obj)) -> do
		ln <- offsetSizeLength @nm @obj mem
		VObj.storeObject @_ @obj ptr ln v)

read :: forall nm obj v sd sm sibfoss .
	(VObj.StoreObject v obj, OffsetSize' nm obj sibfoss) =>
	Device.D sd -> M sm sibfoss -> Memory.M.MapFlags -> IO v
read dvc mem flgs = bracket
	(map @nm @obj dvc mem flgs) (const $ unmap dvc mem)
	(\(ptr :: Ptr (VObj.ObjectType obj)) ->
		VObj.loadObject @_ @obj ptr =<< offsetSizeLength @nm @obj mem)

map :: forall nm obj sd sm sibfoss . OffsetSize' nm obj sibfoss =>
	Device.D sd -> M sm sibfoss -> Memory.M.MapFlags -> IO (Ptr (VObj.ObjectType obj))
map dvc@(Device.D mdvc) m flgs = do
	(_, mm) <- readM'' m
	(ost, sz) <- offsetSize @nm @obj dvc m 0
--	putStrLn "Vk.Memory.map:"
--	putStr "ost: "; print ost
--	putStr "sz : "; print sz
	Memory.M.map mdvc mm ost sz flgs

unmap :: Device.D sd -> M sm sibfoss -> IO ()
unmap (Device.D mdvc) m = do
	(_, mm) <- readM'' m
	Memory.M.unmap mdvc mm
