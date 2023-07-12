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

	M(..), readM'', newM2', writeMBinded',

	-- * IMAGE BUFFER

	ImageBuffer(..), ImageBufferBinded(..), ImageBufferArg(..),

	) where

import Prelude hiding (map, read)
import GHC.TypeLits
import Data.Kind
import Gpu.Vulkan.Object qualified as VObj
import Data.TypeLevel.Tuple.Uncurry
import qualified Data.HeteroParList as HeteroParList
import Data.IORef

import qualified Gpu.Vulkan.Image.Type as Image
import qualified Gpu.Vulkan.Buffer.Type as Buffer
import qualified Gpu.Vulkan.Memory.Middle as M

import qualified Gpu.Vulkan.TypeEnum as T

data M s (sibfoss :: [(Type, ImageBufferArg)]) =
	M (IORef (HeteroParList.PL (U2 ImageBuffer) sibfoss)) M.M

readM'' :: M s sibfoss -> IO (HeteroParList.PL (U2 ImageBuffer) sibfoss, M.M)
readM'' (M ib m) = (, m) <$> readIORef ib

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
