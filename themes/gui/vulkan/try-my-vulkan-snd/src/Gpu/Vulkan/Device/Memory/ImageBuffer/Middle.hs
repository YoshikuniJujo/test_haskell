{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Device.Memory.ImageBuffer.Middle where

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
import qualified Gpu.Vulkan.Device.Middle.Internal as Device.M
import qualified Gpu.Vulkan.Device.Core as Device.C
import qualified Gpu.Vulkan.Memory.Core as Memory.C
import qualified Gpu.Vulkan.Image.Type as Image
import qualified Gpu.Vulkan.Image.Middle as Image.M
import qualified Gpu.Vulkan.Buffer as Buffer
import qualified Gpu.Vulkan.Buffer.Middle as Buffer.M
import qualified Gpu.Vulkan.Device.Memory.ImageBuffer.Kind as K
import qualified Gpu.Vulkan.Device.Memory.AllocateInfo as Device.Memory.Buffer
import qualified Gpu.Vulkan.Memory.Middle as Memory.M

data M s (sibfoss :: [(Type, K.ImageBuffer)]) =
	M (IORef (HeteroVarList (V2 ImageBuffer) sibfoss)) (IORef Memory.C.M)

readM :: M s sibfoss ->
	IO (HeteroVarList (V2 ImageBuffer) sibfoss, Memory.C.M)
readM (M ib r) = (,) <$> readIORef ib <*> readIORef r

readM' (M ib r) = (, r) <$> readIORef ib

writeM :: M s sibfoss -> HeteroVarList (V2 ImageBuffer) sibfoss ->
	Memory.C.M -> IO ()
writeM (M rib r) ibs cm = writeIORef rib ibs >> writeIORef r cm

writeMBinded :: M s sibfoss -> HeteroVarList (V2 (ImageBufferBinded sm)) sibfoss ->
	Memory.C.M -> IO ()
writeMBinded (M rib r) ibs cm = writeIORef rib (heteroVarListMap imageBufferFromBinded ibs) >> writeIORef r cm

writeMBinded' (M rib r) ibs = writeIORef rib (heteroVarListMap imageBufferFromBinded ibs)

imageBufferFromBinded :: V2 (ImageBufferBinded sm) sibfos -> V2 ImageBuffer sibfos
imageBufferFromBinded (V2 (ImageBinded (Image.BindedNew i))) = V2 . Image $ Image.INew i
imageBufferFromBinded (V2 (BufferBinded (Buffer.Binded x b))) = V2 . Buffer $ Buffer.B x b

newM :: HeteroVarList (V2 ImageBuffer) sibfoss ->
	Memory.C.M -> IO (M s sibfoss)
newM ibs cm = M <$> newIORef ibs <*> newIORef cm

newM' ibs cm = (`M` cm) <$> newIORef ibs

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
	Device.M.D -> ImageBuffer sib fos -> IO Memory.M.Requirements
getMemoryRequirements dvc (Buffer (Buffer.B _ b)) =
	Buffer.M.getMemoryRequirements dvc b
getMemoryRequirements dvc (Image (Image.INew i)) =
	Image.M.getMemoryRequirements dvc i

getMemoryRequirementsBinded ::
	Device.M.D -> ImageBufferBinded sm sib fos -> IO Memory.M.Requirements
getMemoryRequirementsBinded dvc (BufferBinded (Buffer.Binded _ b)) =
	Buffer.M.getMemoryRequirements dvc b
getMemoryRequirementsBinded dvc (ImageBinded (Image.BindedNew i)) =
	Image.M.getMemoryRequirements dvc i

memoryRequirementsListToSize ::
	Device.M.Size -> [Memory.M.Requirements] -> Device.M.Size
memoryRequirementsListToSize sz0 [] = sz0
memoryRequirementsListToSize sz0 (reqs : reqss) =
	memoryRequirementsListToSize
		(((sz0 - 1) `div` algn + 1) * algn + sz) reqss
	where
	sz = Memory.M.requirementsSize reqs
	algn = Memory.M.requirementsAlignment reqs

bindImage :: forall sd si nm fmt sm sibfoss .
	Device.M.D -> Image.INew si nm fmt -> M sm sibfoss -> Device.M.Size ->
	IO (Image.BindedNew si sm nm fmt)
bindImage mdvc (Image.INew i) m ost = do
	(_, mm) <- readM m
	Image.M.bindMemory mdvc i (Device.M.MemoryImage mm) ost
	pure (Image.BindedNew i)

rebindImage :: forall sd si sm nm fmt sibfoss .
	Device.M.D -> Image.BindedNew si sm nm fmt -> M sm sibfoss -> Device.M.Size ->
	IO ()
rebindImage mdvc (Image.BindedNew i) m ost = do
	(_, mm) <- readM m
	Image.M.bindMemory mdvc i (Device.M.MemoryImage mm) ost

bindBuffer :: forall sd sb nm objs sm sibfoss .
	Device.M.D -> Buffer.B sb nm objs -> M sm sibfoss -> Device.M.Size ->
	IO (Buffer.Binded sb sm nm objs)
bindBuffer mdvc (Buffer.B lns b) m ost = do
	(_, mm) <- readM' m
	Buffer.M.bindMemory mdvc b (Device.M.Memory mm) ost
	pure (Buffer.Binded lns b)

rebindBuffer :: forall sd sb sm nm objs sibfoss .
	Device.M.D -> Buffer.Binded sb sm nm objs -> M sm sibfoss -> Device.M.Size -> IO ()
rebindBuffer mdvc (Buffer.Binded _lns b) m ost = do
	(_, mm) <- readM' m
	Buffer.M.bindMemory mdvc b (Device.M.Memory mm) ost

map :: forall nm obj sd sm sibfoss .
	Device.M.D -> M sm sibfoss -> Memory.M.MapFlags -> Device.M.Size -> Device.M.Size -> IO (Ptr (ObjectType obj))
map mdvc m flgs ost sz = do
	(_, mm) <- readM' m
	Memory.M.map mdvc (Device.M.Memory mm) ost sz flgs

unmap :: Device.M.D -> M sm sibfoss -> IO ()
unmap mdvc m = do
	(_, mm) <- readM' m
	Memory.M.unmap mdvc (Device.M.Memory mm)
