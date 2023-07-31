{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSet.Middle.Internal (
	D(..), AllocateInfo(..), allocateDs, freeDs,
	Write(..), WriteSources(..), Copy(..),
	updateDs, WriteListToCore, CopyListToCore ) where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Storable.PeekPoke (
	WithPoked, withPoked, withPoked', withPtrS, pattern NullPtr )
import Control.Arrow
import Control.Monad.Cont
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.List (genericLength)
import Data.HeteroParList (pattern (:**))
import Data.HeteroParList qualified as HeteroParList
import Data.Word

import Gpu.Vulkan.Exception.Middle.Internal
import Gpu.Vulkan.Exception.Enum

import qualified Gpu.Vulkan.Device.Middle.Internal as Device
import qualified Gpu.Vulkan.BufferView.Middle.Internal as BufferView
import qualified Gpu.Vulkan.Descriptor.Enum as Descriptor
import qualified Gpu.Vulkan.Descriptor.Middle.Internal as Descriptor
import qualified Gpu.Vulkan.DescriptorPool.Middle.Internal as Pool
import qualified Gpu.Vulkan.DescriptorSetLayout.Middle.Internal as Layout
import qualified Gpu.Vulkan.DescriptorSet.Core as C

import qualified Gpu.Vulkan.Descriptor.Core as Descriptor.C
import qualified Gpu.Vulkan.BufferView.Core as BufferView.C

data AllocateInfo mn = AllocateInfo {
	allocateInfoNext :: TMaybe.M mn,
	allocateInfoDescriptorPool :: Pool.D,
	allocateInfoSetLayouts :: [Layout.D] }

deriving instance Show (TMaybe.M mn) => Show (AllocateInfo mn)

allocateInfoToCore :: WithPoked (TMaybe.M mn) =>
	AllocateInfo mn -> (C.AllocateInfo -> IO a) -> IO ()
allocateInfoToCore AllocateInfo {
	allocateInfoNext = mnxt,
	allocateInfoDescriptorPool = Pool.D pl,
	allocateInfoSetLayouts =
		(((id &&& fromIntegral) `first`) . (length &&& id)) ->
		((dsci, dscw), sls) } f =
	withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') -> do
	psls <- allocaArray dsci \p ->
		p <$ (pokeArray p $ (\(Layout.D l) -> l) <$> sls)
	f C.AllocateInfo {
		C.allocateInfoSType = (),
		C.allocateInfoPNext = pnxt',
		C.allocateInfoDescriptorPool = pl,
		C.allocateInfoDescriptorSetCount = dscw,
		C.allocateInfoPSetLayouts = psls }

newtype D = D C.D deriving Show

allocateDs :: WithPoked (TMaybe.M mn) => Device.D -> AllocateInfo mn -> IO [D]
allocateDs (Device.D dvc) ai = ((D <$>) <$>) . ($ pure) $ runContT do
	let	dsc = length $ allocateInfoSetLayouts ai
	pss <- ContT $ allocaArray dsc
	lift $ allocateInfoToCore ai \fai ->
		withPoked fai \pai -> do
			r <- C.allocateDs dvc pai pss
			throwUnlessSuccess $ Result r
	lift $ peekArray dsc pss

freeDs :: Device.D -> Pool.D -> [D] -> IO ()
freeDs (Device.D dvc) (Pool.D pl) ds = allocaArray ln \pds -> do
	pokeArray pds $ (\(D d) -> d) <$> ds
	r <- C.freeDs dvc pl ln pds
	throwUnlessSuccess $ Result r
	where
	ln :: Integral n => n
	ln = genericLength ds

data Copy mn = Copy {
	copyNext :: TMaybe.M mn,
	copySrcSet :: D,
	copySrcBinding :: Word32,
	copySrcArrayElement :: Word32,
	copyDstSet :: D,
	copyDstBinding :: Word32,
	copyDstArrayElement :: Word32,
	copyDescriptorCount :: Word32 }

deriving instance Show (TMaybe.M mn) => Show (Copy mn)

class CopyListToCore cs where
	copyListToCore ::
		HeteroParList.PL Copy cs -> ([C.Copy] -> IO a) -> IO ()

instance CopyListToCore '[] where
	copyListToCore HeteroParList.Nil f = () <$ f []

instance (WithPoked (TMaybe.M c), CopyListToCore cs) =>
	CopyListToCore (c ': cs) where
	copyListToCore (c :** cs) f =
		copyToCore c \cc -> copyListToCore cs \ccs -> f $ cc : ccs

copyToCore :: WithPoked (TMaybe.M mn) => Copy mn -> (C.Copy -> IO a) -> IO ()
copyToCore Copy {
	copyNext = mnxt,
	copySrcSet = D ss,
	copySrcBinding = sb,
	copySrcArrayElement = sae,
	copyDstSet = D ds,
	copyDstBinding = db,
	copyDstArrayElement = dae,
	copyDescriptorCount = dc } f =
	withPoked' mnxt \pnxt ->
	withPtrS pnxt \(castPtr -> pnxt') -> f C.Copy {
		C.copySType = (),
		C.copyPNext = pnxt',
		C.copySrcSet = ss,
		C.copySrcBinding = sb,
		C.copySrcArrayElement = sae,
		C.copyDstSet = ds,
		C.copyDstBinding = db,
		C.copyDstArrayElement = dae,
		C.copyDescriptorCount = dc }

data Write mn = Write {
	writeNext :: TMaybe.M mn,
	writeDstSet :: D,
	writeDstBinding :: Word32,
	writeDstArrayElement :: Word32,
	writeDescriptorType :: Descriptor.Type,
	writeSources :: WriteSources }

deriving instance Show (TMaybe.M mn) => Show (Write mn)

data WriteSources
	= WriteSourcesInNext Word32
	| WriteSourcesImageInfo [Descriptor.ImageInfo]
	| WriteSourcesBufferInfo [Descriptor.BufferInfo]
	| WriteSourcesBufferView [BufferView.B]
	deriving Show

class WriteListToCore ws where
	writeListToCore ::
		HeteroParList.PL Write ws -> ([C.Write] -> IO a) -> IO ()

instance WriteListToCore '[] where
	writeListToCore HeteroParList.Nil f = () <$ f []

instance (WithPoked (TMaybe.M w), WriteListToCore ws) => WriteListToCore (w ': ws) where
	writeListToCore (w :** ws) f =
		writeToCore w \cw -> writeListToCore ws \cws -> f $ cw : cws

writeToCore :: WithPoked (TMaybe.M mn) => Write mn -> (C.Write -> IO a) -> IO ()
writeToCore Write {
	writeNext = mnxt,
	writeDstSet = D s,
	writeDstBinding = bdg,
	writeDstArrayElement = ae,
	writeDescriptorType = Descriptor.Type tp,
	writeSources = srcs } f =
	withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	writeSourcesToCore srcs \(cnt, pii, pbi, ptbv) ->
	f C.Write {
		C.writeSType = (),
		C.writePNext = pnxt',
		C.writeDstSet = s,
		C.writeDstBinding = bdg,
		C.writeDstArrayElement = ae,
		C.writeDescriptorCount = cnt,
		C.writeDescriptorType = tp,
		C.writePImageInfo = pii,
		C.writePBufferInfo = pbi,
		C.writePTexelBufferView = ptbv }

writeSourcesToCore :: WriteSources -> ((
	Word32, Ptr Descriptor.C.ImageInfo,
	Ptr Descriptor.C.BufferInfo, Ptr BufferView.C.B ) -> IO a) -> IO a
writeSourcesToCore ws f = case ws of
	WriteSourcesInNext c -> f (c, NullPtr, NullPtr, NullPtr)
	WriteSourcesImageInfo (length &&& id -> (ln, iis)) ->
		allocaArray ln \piis ->
		Descriptor.imageInfoToCore `mapM` iis >>= \iis' ->
		pokeArray piis iis' >>
		f (fromIntegral ln, piis, NullPtr, NullPtr)
	WriteSourcesBufferInfo
		(length &&& (Descriptor.bufferInfoToCore <$>) -> (ln, bis)) ->
		allocaArray ln \pbis ->
		pokeArray pbis bis >>
		f (fromIntegral ln, NullPtr, pbis, NullPtr)
	WriteSourcesBufferView
		(length &&& ((\(BufferView.B b) -> b) <$>) -> (ln, bvs)) ->
		allocaArray ln \pbvs ->
		pokeArray pbvs bvs >>
		f (fromIntegral ln, NullPtr, NullPtr, pbvs)

updateDs :: (WriteListToCore ws, CopyListToCore cs) =>
	Device.D ->
	HeteroParList.PL Write ws -> HeteroParList.PL Copy cs ->
	IO ()
updateDs (Device.D dvc) ws cs =
	writeListToCore ws \cws ->
	allocaAndPokeArray cws \(fromIntegral -> wc, pws) ->
	copyListToCore cs \ccs ->
	allocaAndPokeArray ccs \(fromIntegral -> cc, pcs) ->
	C.updateDs dvc wc pws cc pcs

allocaAndPokeArray :: Storable a => [a] -> ((Int, Ptr a) -> IO b) -> IO b
allocaAndPokeArray (length &&& id -> (xc, xs)) f
	= allocaArray xc \p -> pokeArray p xs >> f (xc, p)
