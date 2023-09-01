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

	M(..),

	-- * NEW, READ AND WRITE

	newM, readM, writeMBinded,

	getBinded,

	-- * OBJECT LENGTH

	objectLength

	) where

import Data.Kind
import Data.TypeLevel.Tuple.Uncurry
import Data.HeteroParList qualified as HeteroParList
import Data.IORef

import Gpu.Vulkan.Object qualified as VObj
import Gpu.Vulkan.Buffer.Type qualified as B
import Gpu.Vulkan.Image.Type qualified as I

import Gpu.Vulkan.Memory.ImageBuffer
import Gpu.Vulkan.Memory.Middle qualified as M

-- MEMORY

data M s (ibargs :: [(Type, ImageBufferArg)]) =
	M (IORef (HeteroParList.PL (U2 ImageBuffer) ibargs)) M.M

-- NEW, READ AND WRITE

newM :: HeteroParList.PL (U2 ImageBuffer) ibargs -> M.M -> IO (M s ibargs)
newM ibs mm = (`M` mm) <$> newIORef ibs

getBinded :: M s ibargs -> IO (HeteroParList.PL (U2 (ImageBufferBinded s)) ibargs)
getBinded m = HeteroParList.map toBinded . fst <$> readM m
	where
	toBinded :: U2 ImageBuffer ibarg -> U2 (ImageBufferBinded sm) ibarg
	toBinded (U2 (Image (I.I i))) = U2 . ImageBinded $ I.Binded i
	toBinded (U2 (Buffer (B.B x b))) = U2 . BufferBinded $ B.Binded x b

readM :: M s ibargs -> IO (HeteroParList.PL (U2 ImageBuffer) ibargs, M.M)
readM (M ib m) = (, m) <$> readIORef ib

writeMBinded :: M s ibargs ->
	HeteroParList.PL (U2 (ImageBufferBinded sm)) ibargs -> IO ()
writeMBinded (M rib _r) ibs =
	writeIORef rib (HeteroParList.map fromBinded ibs)
	where
	fromBinded :: U2 (ImageBufferBinded sm) ibarg -> U2 ImageBuffer ibarg
	fromBinded (U2 (ImageBinded (I.Binded i))) = U2 . Image $ I.I i
	fromBinded (U2 (BufferBinded (B.Binded x b))) = U2 . Buffer $ B.B x b

-- OBJECT LENGTH

objectLength :: forall nm obj ibargs sm . ObjectLength nm obj ibargs =>
	M sm ibargs -> IO (VObj.Length obj)
objectLength m = (<$> readM m) \(ibs, _m) -> objectLength' @nm @obj @ibargs ibs
