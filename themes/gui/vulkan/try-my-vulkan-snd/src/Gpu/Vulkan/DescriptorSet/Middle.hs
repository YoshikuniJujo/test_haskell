{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSet.Middle where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Pointable
import Control.Arrow
import Control.Monad.Cont
import Data.Word

import Gpu.Vulkan.Base
import Gpu.Vulkan.Exception
import Gpu.Vulkan.Exception.Enum

import qualified Gpu.Vulkan.Device.Middle as Device
import qualified Gpu.Vulkan.BufferView.Middle.Internal as BufferView
import qualified Gpu.Vulkan.Descriptor.Enum as Descriptor
import qualified Gpu.Vulkan.Descriptor.Middle.Internal as Descriptor
import qualified Gpu.Vulkan.DescriptorPool.Middle as Pool
import qualified Gpu.Vulkan.DescriptorSetLayout.Middle as Layout
import qualified Gpu.Vulkan.DescriptorSet.Core as C

import qualified Gpu.Vulkan.Descriptor.Core as Descriptor.C
import qualified Gpu.Vulkan.BufferView.Core as BufferView.C

data AllocateInfo n = AllocateInfo {
	allocateInfoNext :: Maybe n,
	allocateInfoDescriptorPool :: Pool.P,
	allocateInfoSetLayouts :: [Layout.L] }
	deriving Show

allocateInfoToCore :: Pointable n => AllocateInfo n -> ContT r IO C.AllocateInfo
allocateInfoToCore AllocateInfo {
	allocateInfoNext = mnxt,
	allocateInfoDescriptorPool = Pool.P pl,
	allocateInfoSetLayouts =
		(((id &&& fromIntegral) `first`) . (length &&& id)) ->
		((dsci, dscw), sls)
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	psls <- do
		p <- ContT $ allocaArray dsci
		p <$ lift (pokeArray p $ (\(Layout.L l) -> l) <$> sls)
	pure C.AllocateInfo {
		C.allocateInfoSType = (),
		C.allocateInfoPNext = pnxt,
		C.allocateInfoDescriptorPool = pl,
		C.allocateInfoDescriptorSetCount = dscw,
		C.allocateInfoPSetLayouts = psls }

newtype S = S C.S deriving Show

allocateSs :: Pointable n => Device.D -> AllocateInfo n -> IO [S]
allocateSs (Device.D dvc) ai = ((S <$>) <$>) . ($ pure) $ runContT do
	cai@(C.AllocateInfo_ fai) <- allocateInfoToCore ai
	pai <- ContT $ withForeignPtr fai
	let	dsc = fromIntegral $ C.allocateInfoDescriptorSetCount cai
	pss <- ContT $ allocaArray dsc
	lift do	r <- C.allocateSs dvc pai pss
		throwUnlessSuccess $ Result r
		peekArray dsc pss

data Copy n = Copy {
	copyNext :: Maybe n,
	copySrcSet :: S,
	copySrcBinding :: Word32,
	copySrcArrayElement :: Word32,
	copyDstSet :: S,
	copyDstBinding :: Word32,
	copyDstArrayElement :: Word32,
	copyDescriptorCount :: Word32 }
	deriving Show

copyToCore :: Pointable n => Copy n -> ContT r IO C.Copy
copyToCore Copy {
	copyNext = mnxt,
	copySrcSet = S ss,
	copySrcBinding = sb,
	copySrcArrayElement = sae,
	copyDstSet = S ds,
	copyDstBinding = db,
	copyDstArrayElement = dae,
	copyDescriptorCount = dc
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	pure C.Copy {
		C.copySType = (),
		C.copyPNext = pnxt,
		C.copySrcSet = ss,
		C.copySrcBinding = sb,
		C.copySrcArrayElement = sae,
		C.copyDstSet = ds,
		C.copyDstBinding = db,
		C.copyDstArrayElement = dae,
		C.copyDescriptorCount = dc }

data Write n = Write {
	writeNext :: Maybe n,
	writeDstSet :: S,
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

writeToCore :: Pointable n => Write n -> ContT r IO C.Write
writeToCore Write {
	writeNext = mnxt,
	writeDstSet = S s,
	writeDstBinding = bdg,
	writeDstArrayElement = ae,
	writeDescriptorType = Descriptor.Type tp,
	writeSources = srcs
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	(cnt, pii, pbi, ptbv) <- writeSourcesToCore srcs
	pure C.Write {
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

writeSourcesToCore :: WriteSources -> ContT r IO (
	Word32, Ptr Descriptor.C.ImageInfo,
	Ptr Descriptor.C.BufferInfo, Ptr BufferView.C.B )
writeSourcesToCore = \case
	WriteSourcesInNext c -> pure (c, NullPtr, NullPtr, NullPtr)
	WriteSourcesImageInfo (length &&& id -> (ln, iis)) -> do
		piis <- ContT $ allocaArray ln
		iis' <- lift $ Descriptor.imageInfoToCore `mapM` iis
		lift $ pokeArray piis iis'
		pure (fromIntegral ln, piis, NullPtr, NullPtr)
	WriteSourcesBufferInfo
		(length &&& (Descriptor.bufferInfoToCore <$>) -> (ln, bis)) ->
		do	pbis <- ContT $ allocaArray ln
			lift $ pokeArray pbis bis
			pure (fromIntegral ln, NullPtr, pbis, NullPtr)
	WriteSourcesBufferView
		(length &&& ((\(BufferView.B b) -> b) <$>) -> (ln, bvs)) -> do
		pbvs <- ContT $ allocaArray ln
		lift $ pokeArray pbvs bvs
		pure (fromIntegral ln, NullPtr, NullPtr, pbvs)

updateDs :: (Pointable n, Pointable n') =>
	Device.D -> [Write n] -> [Copy n'] -> IO ()
updateDs (Device.D dvc) ws cs = ($ pure) $ runContT do
	ws' <- writeToCore `mapM` ws
	cs' <- copyToCore `mapM` cs
	(fromIntegral -> wc, pws) <- allocaAndPokeArray ws'
	(fromIntegral -> cc, pcs) <- allocaAndPokeArray cs'
	lift $ C.updateSs dvc wc pws cc pcs
