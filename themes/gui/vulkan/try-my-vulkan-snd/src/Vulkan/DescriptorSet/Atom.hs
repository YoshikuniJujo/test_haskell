{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.DescriptorSet.Atom where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Pointable
import Control.Arrow
import Control.Monad.Cont
import Data.Word

import Vulkan.Base

import qualified Foreign.Storable.Generic

import qualified Vulkan.Device.Middle as Device
import qualified Vulkan.Buffer.View as Buffer.View
import qualified Vulkan.Descriptor.Enum as Dsc
import qualified Vulkan.Descriptor.Atom as Dsc
import qualified Vulkan.DescriptorSet.Core as C

import qualified Vulkan.Descriptor.Middle as M

import qualified Vulkan.DescriptorSet.Middle as M

data Write n v = Write {
	writeNext :: Maybe n,
	writeDstSet :: M.S,
	writeDstBinding :: Word32,
	writeDstArrayElement :: Word32,
	writeDescriptorType :: Dsc.Type,
	writeImageBufferInfoTexelBufferViews ::
		Either Word32 (ImageBufferInfoTexelBufferViews v) }
	deriving Show

data ImageBufferInfoTexelBufferViews v
	= ImageInfos [M.ImageInfo]
	| BufferInfos [Dsc.BufferInfo v]
	| TexelBufferViews [Buffer.View.V]
	deriving Show

writeToCore :: (Pointable n, Storable (Foreign.Storable.Generic.Wrap v)) =>
	Write n v -> ContT r IO C.Write
writeToCore Write {
	writeNext = mnxt,
	writeDstSet = M.S s,
	writeDstBinding = b,
	writeDstArrayElement = ae,
	writeDescriptorType = Dsc.Type tp,
	writeImageBufferInfoTexelBufferViews = mibitbvs
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	(dc, piis, pbis, ptbvs) <- case mibitbvs of
		Left c -> pure (c, NullPtr, NullPtr, NullPtr)
		Right (ImageInfos ((M.imageInfoToCore <$>) -> ciis)) -> do
			(iic, p) <- allocaAndPokeArray ciis
			pure (fromIntegral iic, p, NullPtr, NullPtr)
		Right (BufferInfos ((Dsc.bufferInfoToCore <$>) -> cbis)) -> do
			(bic, p) <- allocaAndPokeArray cbis
			pure (fromIntegral bic, NullPtr, p, NullPtr)
		Right (TexelBufferViews tbvs) -> do
			let	ctbvs = (\(Buffer.View.V v) -> v) <$> tbvs
			(tbvc, p) <- allocaAndPokeArray ctbvs
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

updateSs :: (Pointable n, Pointable n',
		Storable (Foreign.Storable.Generic.Wrap v)) =>
	Device.D -> [Write n v] -> [M.Copy n'] -> IO ()
updateSs (Device.D dvc) (length &&& id -> (wc, ws))
	(length &&& id -> (cc, cs)) = ($ pure) $ runContT do
	pws <- ContT $ allocaArray wc
	lift . pokeArray pws =<< writeToCore `mapM` ws
	pcs <- ContT $ allocaArray cc
	lift . pokeArray pcs =<< M.copyToCore `mapM` cs
	lift $ C.updateSs dvc (fromIntegral wc) pws (fromIntegral cc) pcs
