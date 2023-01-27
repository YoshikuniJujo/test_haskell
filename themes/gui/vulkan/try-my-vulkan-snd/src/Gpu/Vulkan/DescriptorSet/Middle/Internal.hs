{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSet.Middle.Internal (
	D(..), AllocateInfo(..), allocateDs,
	Write(..), WriteSources(..), Copy(..), updateDs
	) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Storable.PeekPoke
import Control.Arrow
import Control.Monad.Cont
import Data.Word

import Gpu.Vulkan.Exception.Middle.Internal
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.Misc.Middle.Internal

import qualified Gpu.Vulkan.Device.Middle.Internal as Device
import qualified Gpu.Vulkan.BufferView.Middle.Internal as BufferView
import qualified Gpu.Vulkan.Descriptor.Enum as Descriptor
import qualified Gpu.Vulkan.Descriptor.Middle.Internal as Descriptor
import qualified Gpu.Vulkan.DescriptorPool.Middle.Internal as Pool
import qualified Gpu.Vulkan.DescriptorSetLayout.Middle.Internal as Layout
import qualified Gpu.Vulkan.DescriptorSet.Core as C

import qualified Gpu.Vulkan.Descriptor.Core as Descriptor.C
import qualified Gpu.Vulkan.BufferView.Core as BufferView.C

data AllocateInfo n = AllocateInfo {
	allocateInfoNext :: Maybe n,
	allocateInfoDescriptorPool :: Pool.D,
	allocateInfoSetLayouts :: [Layout.L] }
	deriving Show

allocateInfoToCore ::
	WithPoked n => AllocateInfo n -> (C.AllocateInfo -> IO a) -> IO ()
allocateInfoToCore AllocateInfo {
	allocateInfoNext = mnxt,
	allocateInfoDescriptorPool = Pool.D pl,
	allocateInfoSetLayouts =
		(((id &&& fromIntegral) `first`) . (length &&& id)) ->
		((dsci, dscw), sls) } f =
	withPokedMaybe' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') -> do
	psls <- allocaArray dsci \p ->
		p <$ (pokeArray p $ (\(Layout.L l) -> l) <$> sls)
	f C.AllocateInfo {
		C.allocateInfoSType = (),
		C.allocateInfoPNext = pnxt',
		C.allocateInfoDescriptorPool = pl,
		C.allocateInfoDescriptorSetCount = dscw,
		C.allocateInfoPSetLayouts = psls }

newtype D = D C.S deriving Show

allocateDs :: WithPoked n => Device.D -> AllocateInfo n -> IO [D]
allocateDs (Device.D dvc) ai = ((D <$>) <$>) . ($ pure) $ runContT do
	let	dsc = fromIntegral . length $ allocateInfoSetLayouts ai
	pss <- ContT $ allocaArray dsc
	lift $ allocateInfoToCore ai \(C.AllocateInfo_ fai) ->
		withForeignPtr fai \pai -> do
			r <- C.allocateSs dvc pai pss
			throwUnlessSuccess $ Result r
	lift $ peekArray dsc pss

data Copy n = Copy {
	copyNext :: Maybe n,
	copySrcSet :: D,
	copySrcBinding :: Word32,
	copySrcArrayElement :: Word32,
	copyDstSet :: D,
	copyDstBinding :: Word32,
	copyDstArrayElement :: Word32,
	copyDescriptorCount :: Word32 }
	deriving Show

copyToCore :: WithPoked n => Copy n -> (C.Copy -> IO a) -> IO ()
copyToCore Copy {
	copyNext = mnxt,
	copySrcSet = D ss,
	copySrcBinding = sb,
	copySrcArrayElement = sae,
	copyDstSet = D ds,
	copyDstBinding = db,
	copyDstArrayElement = dae,
	copyDescriptorCount = dc } f =
	withPokedMaybe' mnxt \pnxt ->
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

data Write n = Write {
	writeNext :: Maybe n,
	writeDstSet :: D,
	writeDstBinding :: Word32,
	writeDstArrayElement :: Word32,
	writeDescriptorType :: Descriptor.Type,
	writeSources :: WriteSources }
	deriving Show

data WriteSources
	= WriteSourcesInNext Word32
	| WriteSourcesImageInfo [Descriptor.ImageInfo]
	| WriteSourcesBufferInfo [Descriptor.BufferInfo]
	| WriteSourcesBufferView [BufferView.B]
	deriving Show

writeToCore :: Pokable n => Write n -> (C.Write -> IO a) -> IO a
writeToCore Write {
	writeNext = mnxt,
	writeDstSet = D s,
	writeDstBinding = bdg,
	writeDstArrayElement = ae,
	writeDescriptorType = Descriptor.Type tp,
	writeSources = srcs
	} f = withPokedMaybe mnxt \(castPtr -> pnxt) ->
	writeSourcesToCore srcs \(cnt, pii, pbi, ptbv) ->
	f C.Write {
		C.writeSType = (),
		C.writePNext = pnxt,
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

updateDs :: (Pokable w, WithPoked c) =>
	Device.D -> [Write w] -> [Copy c] -> IO ()
updateDs (Device.D dvc) ws cs = ($ pure) $ runContT do
	ws' <- (ContT . writeToCore) `mapM` ws
	(fromIntegral -> wc, pws) <- allocaAndPokeArray ws'

	lift $ (copyToCore `mapContM` cs) \cs' ->
		allocaAndPokeArray' cs' \(fromIntegral -> cc, pcs) ->
		C.updateSs dvc wc pws cc pcs
