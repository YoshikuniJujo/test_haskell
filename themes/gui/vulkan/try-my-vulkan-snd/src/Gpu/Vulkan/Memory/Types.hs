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

module Gpu.Vulkan.Memory.Types (

	-- * MEMORY

	M(..), readM, newM2', writeMBinded',

	-- * IMAGE BUFFER

	ImageBuffer(..), ImageBufferBinded(..), ImageBufferArg(..),
	Alignments(..),

	-- * OBJECT LENGTH

	objectLength, ObjectLength,

	-- * OTHERS

	adjustOffsetSize

	) where

import Prelude hiding (map, read)
import GHC.TypeLits
import Data.Kind
import Gpu.Vulkan.Object qualified as VObj
import Data.TypeLevel.Tuple.Uncurry
import Data.HeteroParList qualified as HeteroParList
import Data.HeteroParList (pattern (:**))
import Data.IORef

import qualified Gpu.Vulkan.Image.Type as Image
import qualified Gpu.Vulkan.Buffer.Type as Buffer
import qualified Gpu.Vulkan.Memory.Middle as M

import qualified Gpu.Vulkan.TypeEnum as T

import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Device.Middle as Device.M

import qualified Gpu.Vulkan.Image.Type as Image
import qualified Gpu.Vulkan.Image.Middle as Image.M
import qualified Gpu.Vulkan.Buffer.Type as Buffer
import qualified Gpu.Vulkan.Buffer.Middle as Buffer.M
import qualified Gpu.Vulkan.Memory.Middle as Memory.M

data M s (sibfoss :: [(Type, ImageBufferArg)]) =
	M (IORef (HeteroParList.PL (U2 ImageBuffer) sibfoss)) M.M

readM :: M s sibfoss -> IO (HeteroParList.PL (U2 ImageBuffer) sibfoss, M.M)
readM (M ib m) = (, m) <$> readIORef ib

writeMBinded' :: M s sibfoss ->
	HeteroParList.PL (U2 (ImageBufferBinded sm)) sibfoss -> IO ()
writeMBinded' (M rib _r) ibs = writeIORef rib (HeteroParList.map imageBufferFromBinded ibs)

imageBufferFromBinded :: U2 (ImageBufferBinded sm) sibfos -> U2 ImageBuffer sibfos
imageBufferFromBinded (U2 (ImageBinded (Image.Binded i))) = U2 . Image $ Image.I i
imageBufferFromBinded (U2 (BufferBinded (Buffer.Binded x b))) = U2 . Buffer $ Buffer.B x b

newM2' :: HeteroParList.PL (U2 ImageBuffer) sibfoss -> M.M -> IO (M s sibfoss)
newM2' ibs mm = (`M` mm) <$> newIORef ibs

-- deriving instance Show (HeteroParList.PL (U2 ImageBuffer) sibfoss) =>
--	Show (M s sibfoss)

data ImageBuffer sib (ib :: ImageBufferArg) where
	Image :: Image.I si nm fmt -> ImageBuffer si ('ImageArg nm fmt)
	Buffer :: Buffer.B sb nm objs -> ImageBuffer sb ('BufferArg nm objs)

deriving instance Show (Image.I sib nm fmt) =>
	Show (ImageBuffer sib ('ImageArg nm fmt))

deriving instance Show (HeteroParList.PL VObj.ObjectLength objs) =>
	Show (ImageBuffer sib ('BufferArg nm objs))

data ImageBufferBinded sm sib (ib :: ImageBufferArg) where
	ImageBinded :: Image.Binded sm si nm fmt ->
		ImageBufferBinded sm si ('ImageArg nm fmt)
	BufferBinded :: Buffer.Binded sm sb nm objs ->
		ImageBufferBinded sm sb ('BufferArg nm objs)

data ImageBufferArg = ImageArg Symbol T.Format | BufferArg Symbol [VObj.Object]

class Alignments (ibs :: [(Type, ImageBufferArg)]) where
	alignments :: [Maybe Int]

instance Alignments '[] where alignments = []

instance Alignments ibs =>
	Alignments ('(_s, 'ImageArg _nm _fmt) ': ibs) where
	alignments = Nothing : alignments @ibs

instance (VObj.SizeAlignment obj, Alignments ibs) =>
	Alignments ('(_s, 'BufferArg _nm (obj ': _objs)) ': ibs) where
	alignments = Just (VObj.objectAlignment @obj) : alignments @ibs

objectLength :: forall nm obj ibargs sm . ObjectLength nm obj ibargs =>
	M sm ibargs -> IO (VObj.ObjectLength obj)
objectLength m = (<$> readM m) \(ibs, _m) -> objectLength' @nm @obj @ibargs ibs

class ObjectLength (nm :: Symbol) (obj :: VObj.Object) ibargs where
	objectLength' :: HeteroParList.PL (U2 ImageBuffer) ibargs ->
		VObj.ObjectLength obj

instance VObj.ObjectLengthOf obj objs =>
	ObjectLength nm obj ('(sib, 'BufferArg nm objs) ': ibargs) where
	objectLength' (U2 (Buffer (Buffer.B lns _)) :** _) =
		VObj.objectLengthOf @obj lns

instance {-# OVERLAPPABLE #-} ObjectLength nm obj ibargs =>
	ObjectLength nm obj (ibarg ': ibargs) where
	objectLength' (_ :** lns) = objectLength' @nm @obj lns

adjustOffsetSize :: Device.D sd -> ImageBuffer sib ibarg -> Device.M.Size ->
	IO (Device.M.Size, Device.M.Size)
adjustOffsetSize dvc ib ost = do
	reqs <- getMemoryRequirements dvc ib
	let	algn = Memory.M.requirementsAlignment reqs
		sz = Memory.M.requirementsSize reqs
	pure (((ost - 1) `div` algn + 1) * algn, sz)

getMemoryRequirements ::
	Device.D sd -> ImageBuffer sib fos -> IO Memory.M.Requirements
getMemoryRequirements (Device.D dvc) (Buffer (Buffer.B _ b)) =
	Buffer.M.getMemoryRequirements dvc b
getMemoryRequirements (Device.D dvc) (Image (Image.I i)) =
	Image.M.getMemoryRequirements dvc i
