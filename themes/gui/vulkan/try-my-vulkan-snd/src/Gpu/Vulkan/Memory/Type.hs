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

module Gpu.Vulkan.Memory.Type (

	-- * TYPE

	M,

	-- * NEW, READ AND WRITE

	newM, readM, writeMBinded,

	-- * OBJECT LENGTH

	objectLength
	) where

import Data.Kind
import Data.TypeLevel.Tuple.Uncurry
import Data.HeteroParList qualified as HeteroParList
import Data.IORef

import Gpu.Vulkan.Object qualified as VObj
import Gpu.Vulkan.Buffer.Type qualified as Buffer
import Gpu.Vulkan.Image.Type qualified as Image

import Gpu.Vulkan.Memory.ImageBuffer
import Gpu.Vulkan.Memory.Middle qualified as M

data M s (ibargs :: [(Type, ImageBufferArg)]) =
	M (IORef (HeteroParList.PL (U2 ImageBuffer) ibargs)) M.M

readM :: M s ibargs -> IO (HeteroParList.PL (U2 ImageBuffer) ibargs, M.M)
readM (M ib m) = (, m) <$> readIORef ib

writeMBinded :: M s ibargs ->
	HeteroParList.PL (U2 (ImageBufferBinded sm)) ibargs -> IO ()
writeMBinded (M rib _r) ibs = writeIORef rib (HeteroParList.map imageBufferFromBinded ibs)

imageBufferFromBinded :: U2 (ImageBufferBinded sm) sibfos -> U2 ImageBuffer sibfos
imageBufferFromBinded (U2 (ImageBinded (Image.Binded i))) = U2 . Image $ Image.I i
imageBufferFromBinded (U2 (BufferBinded (Buffer.Binded x b))) = U2 . Buffer $ Buffer.B x b

newM :: HeteroParList.PL (U2 ImageBuffer) ibargs -> M.M -> IO (M s ibargs)
newM ibs mm = (`M` mm) <$> newIORef ibs

objectLength :: forall nm obj ibargs sm . ObjectLength nm obj ibargs =>
	M sm ibargs -> IO (VObj.ObjectLength obj)
objectLength m = (<$> readM m) \(ibs, _m) -> objectLength' @nm @obj @ibargs ibs
