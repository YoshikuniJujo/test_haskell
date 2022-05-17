{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Descriptor.Set where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Pointable
import Control.Arrow
import Control.Monad.Cont
import Data.Word

import qualified Foreign.Storable.Generic

import Vulkan.Exception
import Vulkan.Exception.Enum

import qualified Vulkan.Device as Device
import qualified Vulkan.Buffer.View as Buffer.View
import qualified Vulkan.Descriptor.Enum as Dsc
import qualified Vulkan.Descriptor as Dsc
import qualified Vulkan.Descriptor.Pool as Pool
import qualified Vulkan.Descriptor.Set.Layout as Layout
import qualified Vulkan.Descriptor.Set.Core as C

data AllocateInfo n = AllocateInfo {
	allocateInfoNext :: Maybe n,
	allocateInfoDescriptorPool :: Pool.P,
	allocateInfoDescriptorSetCountOrSetLayouts :: Either Word32 [Layout.L] }
	deriving Show

allocateInfoToCore :: Pointable n => AllocateInfo n -> ContT r IO C.AllocateInfo
allocateInfoToCore AllocateInfo {
	allocateInfoNext = mnxt,
	allocateInfoDescriptorPool = Pool.P pl,
	allocateInfoDescriptorSetCountOrSetLayouts = either
		((, Nothing) . (fromIntegral &&& id))
		(((id &&& fromIntegral) `first`) . (length &&& Just)) ->
		((dsci, dscw), msls)
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	psls <- case msls of
		Nothing -> pure NullPtr
		Just sls -> do
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

data Write n v = Write {
	writeNext :: Maybe n,
	writeDstSet :: S,
	writeDstBinding :: Word32,
	writeDstArrayElement :: Word32,
	writeDescriptorType :: Dsc.Type,
	writeImageBufferInfoTexelBufferViews ::
		Either Word32 (ImageBufferInfoTexelBufferViews v) }
	deriving Show

data ImageBufferInfoTexelBufferViews v
	= ImageInfos [Dsc.ImageInfo]
	| BufferInfos [Dsc.BufferInfo v]
	| TexelBufferViews [Buffer.View.V]
	deriving Show

writeToCore :: (Pointable n, Storable (Foreign.Storable.Generic.Wrap v)) =>
	Write n v -> ContT r IO C.Write
writeToCore Write {
	writeNext = mnxt,
	writeDstSet = S s,
	writeDstBinding = b,
	writeDstArrayElement = ae,
	writeDescriptorType = Dsc.Type tp,
	writeImageBufferInfoTexelBufferViews = mibitbvs
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	(dc, piis, pbis, ptbvs) <- case mibitbvs of
		Left c -> pure (c, NullPtr, NullPtr, NullPtr)
		Right (ImageInfos (length &&& id -> (iic, iis))) -> do
			let	ciis = Dsc.imageInfoToCore <$> iis
			p <- ContT $ allocaArray iic
			lift $ pokeArray p ciis
			pure (fromIntegral iic, p, NullPtr, NullPtr)
		Right (BufferInfos (length &&& id -> (bic, bis))) -> do
			let	cbis = Dsc.bufferInfoToCore <$> bis
			p <- ContT $ allocaArray bic
			lift $ pokeArray p cbis
			pure (fromIntegral bic, NullPtr, p, NullPtr)
		Right (TexelBufferViews (length &&& id -> (tbvc, tbvs))) -> do
			let	ctbvs = (\(Buffer.View.V v) -> v) <$> tbvs
			p <- ContT $ allocaArray tbvc
			lift $ pokeArray p ctbvs
			pure (fromIntegral tbvc, NullPtr, NullPtr, p)
	pure C.Write {
		C.writeSType = (),
		C.writePNext = pnxt,
		C.writeDstSet = s,
		C.writeDstBinding = b,
		C.writeDstArrayElement = ae,
		C.writeDescriptorCount = dc,
		C.writeDescriptorType = tp,
		C.writePImageInfo = piis,
		C.writePBufferInfo = pbis,
		C.writePTexelBufferView = ptbvs }
