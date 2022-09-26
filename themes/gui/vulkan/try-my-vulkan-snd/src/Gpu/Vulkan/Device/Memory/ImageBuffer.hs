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

import Prelude hiding (map)
import GHC.TypeLits
import Foreign.Ptr
import Foreign.Pointable
import Control.Exception hiding (try)
import Data.Kind
import Data.Kind.Object hiding (Offset(..))
import Data.HeteroList
import Data.IORef

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
	M (IORef (HeteroVarList (V2 ImageBuffer) sibfoss, Device.C.Memory))

readM :: M s sibfoss ->
	IO (HeteroVarList (V2 ImageBuffer) sibfoss, Device.C.Memory)
readM (M r) = readIORef r

writeM :: M s sibfoss -> HeteroVarList (V2 ImageBuffer) sibfoss ->
	Device.C.Memory -> IO ()
writeM (M r) ibs cm = writeIORef r (ibs, cm)

newM :: HeteroVarList (V2 ImageBuffer) sibfoss ->
	Device.C.Memory -> IO (M s sibfoss)
newM ibs cm = M <$> newIORef (ibs, cm)

-- deriving instance Show (HeteroVarList (V2 ImageBuffer) sibfoss) =>
--	Show (M s sibfoss)

data ImageBuffer sib (ib :: K.ImageBuffer) where
	Image :: Image.INew si nm fmt -> ImageBuffer si ('K.Image nm fmt)
	Buffer :: Buffer.B sb nm objs -> ImageBuffer sb ('K.Buffer nm objs)

deriving instance Show (Image.INew sib nm fmt) =>
	Show (ImageBuffer sib ('K.Image nm fmt))

deriving instance Show (HeteroVarList ObjectLength objs) =>
	Show (ImageBuffer sib ('K.Buffer nm objs))

data ImageBufferBinded sm sib (ib :: K.ImageBuffer) where
	ImageBinded :: Image.BindedNew si sm nm fmt ->
		ImageBufferBinded sm si ('K.Image nm fmt)
	BufferBinded :: Buffer.Binded sb sm nm objs ->
		ImageBufferBinded sm sb ('K.Buffer nm objs)

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
	\(Device.M.Memory mem) -> f =<< newM bs mem

reallocate :: (
	Pointable n, Pointable c, Pointable d ) =>
	Device.D sd -> HeteroVarList (V2 ImageBuffer) sibfoss ->
	Device.Memory.Buffer.AllocateInfo n ->
	Maybe (AllocationCallbacks.A c) ->
	Maybe (AllocationCallbacks.A d) -> M s sibfoss -> IO ()
reallocate dvc@(Device.D mdvc) bs ai macc macd mem = do
	mai <- allocateInfoToMiddle dvc bs ai
	Device.M.Memory newmem <- Memory.M.allocate mdvc mai macc
	(_, oldmem) <- readM mem
	Memory.M.free mdvc (Device.M.Memory oldmem) macd
	writeM mem bs newmem

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

instance (Offset sb ('K.Buffer nm objs) sibfoss', BindAll fibfoss sibfoss') =>
	BindAll ('(sb, ('K.Buffer nm objs)) ': fibfoss) sibfoss' where
	bindAll dvc (V2 (Buffer bf) :...: ibs) m = (:...:)
		<$> (V2 . BufferBinded <$> bindBuffer dvc bf m)
		<*> bindAll dvc ibs m

bindImage :: forall sd si nm fmt sm sibfoss . Offset si ('K.Image nm fmt) sibfoss =>
	Device.D sd -> Image.INew si nm fmt -> M sm sibfoss ->
	IO (Image.BindedNew si sm nm fmt)
bindImage dvc@(Device.D mdvc) (Image.INew i) m = do
	(_, mm) <- readM m
	ost <- offset @si @('K.Image nm fmt) dvc m 0
	Image.M.bindMemory mdvc i (Device.M.MemoryImage mm) ost
	pure (Image.BindedNew i)

bindBuffer :: forall sd sb nm objs sm sibfoss . Offset sb ('K.Buffer nm objs) sibfoss =>
	Device.D sd -> Buffer.B sb nm objs -> M sm sibfoss ->
	IO (Buffer.Binded sb sm nm objs)
bindBuffer dvc@(Device.D mdvc) (Buffer.B lns b) m = do
	(_, mm) <- readM m
	ost <- offset @sb @('K.Buffer nm objs) dvc m 0
	Buffer.M.bindMemory mdvc (Buffer.M.B b) (Device.M.Memory mm) ost
	pure (Buffer.Binded lns b)

class Offset
	sib (ib :: K.ImageBuffer) (sibfoss :: [(Type, K.ImageBuffer)]) where
	offset :: Device.D sd -> M sm sibfoss -> Device.M.Size -> IO Device.M.Size

instance Offset sib ib ('(sib, ib) ': sibfoss) where
	offset dvc m ost = do
		(ib :...: _ibs, _m) <- readM m
		reqs <- getMemoryRequirements' dvc ib
		let	algn = Memory.M.requirementsAlignment reqs
		pure $ ((ost - 1) `div` algn + 1) * algn

instance {-# OVERLAPPABLE #-} Offset sib ib sibfoss =>
	Offset sib ib ('(sib', ib') ': sibfoss) where
	offset dvc m_ ost = do
		(ib :...: ibs, m) <- readM m_
		reqs <- getMemoryRequirements' dvc ib
		let	sz = Memory.M.requirementsSize reqs
			algn = Memory.M.requirementsAlignment reqs
		m' <- newM ibs m
		offset @sib @ib dvc m'
			$ ((ost - 1) `div` algn + 1) * algn + sz

class Try (nm :: Symbol) sibfoss where
	try :: Device.D sd -> M sm sibfoss -> IO (Device.M.Size, Device.M.Size)

instance Try nm ('(sib, 'K.Buffer nm objs) ': sibfoss) where
	try dvc m = do
		(ib :...: _, _) <- readM m
		reqs <- getMemoryRequirements' dvc ib
		let	sz = Memory.M.requirementsSize reqs
			algn = Memory.M.requirementsAlignment reqs
		pure (sz, algn)

instance {-# OVERLAPPABLE #-} Try nm sibfoss =>
	Try nm (_t ': sibfoss) where
	try dvc m_ = do
		(_ :...: ibs, m) <- readM m_
		try @nm dvc =<< newM ibs m

class OffsetSize (nm :: Symbol) (obj :: Object) sibfoss where
	offsetSize :: Device.D sd -> M sm sibfoss ->
		Device.M.Size -> IO (Device.M.Size, Device.M.Size)
	offsetSizeLength :: M sm sibfoss -> IO (ObjectLength obj)

instance OffsetSizeObject obj objs =>
	OffsetSize nm obj ('(sib, 'K.Buffer nm objs) ': sibfoss) where
	offsetSize dvc m ost = do
		(ib@(V2 (Buffer (Buffer.B lns _))) :...: _ibs, _m) <- readM m
		reqs <- getMemoryRequirements' dvc ib
		let	algn = Memory.M.requirementsAlignment reqs
		pure $ offsetSizeObject @obj
			(((ost - 1) `div` algn + 1) * algn) lns
	offsetSizeLength m = do
		(V2 (Buffer (Buffer.B lns _)) :...: _, _) <- readM m
		pure $ offsetSizeObjectLength @obj lns

instance {-# OVERLAPPABLE #-}
	OffsetSize nm obj sibfoss =>
	OffsetSize nm obj ('(sib, ib) ': sibfoss) where
	offsetSize dvc m_ ost = do
		(ib :...: ibs, m) <- readM m_
		reqs <- getMemoryRequirements' dvc ib
		let	algn = Memory.M.requirementsAlignment reqs
			sz = Memory.M.requirementsSize reqs
		m' <- newM ibs m
		offsetSize @nm @obj dvc m'
			$ ((ost - 1) `div` algn + 1) * algn + sz
	offsetSizeLength m_ = do
		(_ :...: lns, m) <- readM m_
		offsetSizeLength @nm @obj =<< newM lns m

class OffsetSizeObject (obj :: Object) (objs :: [Object]) where
	offsetSizeObject :: Device.M.Size -> HeteroVarList ObjectLength objs ->
		(Device.M.Size, Device.M.Size)
	offsetSizeObjectLength :: HeteroVarList ObjectLength objs ->
		ObjectLength obj

instance SizeAlignment obj => OffsetSizeObject obj (obj ': objs) where
	offsetSizeObject n (ln :...: _) = (ost, fromIntegral $ objectSize ln)
		where
		ost = ((n - 1) `div` algn + 1) * algn
		algn = fromIntegral (objectAlignment @obj)
	offsetSizeObjectLength (ln :...: _) = ln

instance {-# OVERLAPPABLE #-} (
	SizeAlignment obj, SizeAlignment obj',
	OffsetSizeObject obj objs ) =>
	OffsetSizeObject obj (obj' ': objs) where
	offsetSizeObject n (ln :...: lns) =
		offsetSizeObject @obj (ost + fromIntegral (objectSize ln)) lns
		where
		ost = ((n - 1) `div` algn + 1) * algn
		algn = fromIntegral (objectAlignment @obj)
	offsetSizeObjectLength (_ :...: lns) = offsetSizeObjectLength @obj lns

write :: forall nm obj sd sm sibfoss v .
	(StoreObject v obj, OffsetSize nm obj sibfoss) =>
	Device.D sd -> M sm sibfoss -> Memory.M.MapFlags -> v -> IO ()
write dvc mem flgs v = bracket
	(map @nm @obj dvc mem flgs) (const $ unmap dvc mem)
	(\(ptr :: Ptr (ObjectType obj)) -> do
		ln <- offsetSizeLength @nm @obj mem
		storeObject @_ @obj ptr ln v)

read :: forall nm obj v sd sm sibfoss .
	(StoreObject v obj, OffsetSize nm obj sibfoss) =>
	Device.D sd -> M sm sibfoss -> Memory.M.MapFlags -> IO v
read dvc mem flgs = bracket
	(map @nm @obj dvc mem flgs) (const $ unmap dvc mem)
	(\(ptr :: Ptr (ObjectType obj)) ->
		loadObject @_ @obj ptr =<< offsetSizeLength @nm @obj mem)

map :: forall nm obj sd sm sibfoss . OffsetSize nm obj sibfoss =>
	Device.D sd -> M sm sibfoss -> Memory.M.MapFlags -> IO (Ptr (ObjectType obj))
map dvc@(Device.D mdvc) m flgs = do
	(_, mm) <- readM m
	(ost, sz) <- offsetSize @nm @obj dvc m 0
	Memory.M.map mdvc (Device.M.Memory mm) ost sz flgs

unmap :: Device.D sd -> M sm sibfoss -> IO ()
unmap (Device.D mdvc) m = do
	(_, mm) <- readM m
	Memory.M.unmap mdvc (Device.M.Memory mm)
