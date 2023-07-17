{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Memory.Type where

import GHC.TypeLits
import Data.Kind
import Data.TypeLevel.Tuple.Uncurry
import Data.HeteroParList qualified as HeteroParList
import Data.HeteroParList (pattern (:**))
import Data.IORef

import Gpu.Vulkan.Object qualified as VObj
import Gpu.Vulkan.Buffer.Type qualified as Buffer
import Gpu.Vulkan.Image.Type qualified as Image

import Gpu.Vulkan.Memory.ImageBuffer
import Gpu.Vulkan.Memory.Middle qualified as M

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
