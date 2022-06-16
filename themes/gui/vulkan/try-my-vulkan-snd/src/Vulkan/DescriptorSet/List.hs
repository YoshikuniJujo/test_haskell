{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds, GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.DescriptorSet.List where

import Foreign.Ptr
import Foreign.Pointable
import Control.Monad.Cont
import Data.HeteroList
import Data.Word

import Vulkan.Base

import qualified Vulkan.Device.Type as Device
import qualified Vulkan.Device.Middle as Device.M
import qualified Vulkan.BufferView as Buffer.View
import qualified Vulkan.Descriptor.List as Dsc
import qualified Vulkan.Descriptor.Enum as Dsc
import qualified Vulkan.Descriptor.Middle as Dsc.M
import qualified Vulkan.DescriptorSet as Dsc.Set
import qualified Vulkan.DescriptorSet.Middle as Dsc.Set.M
import qualified Vulkan.DescriptorSet.Core as C

data Write n sd sp sl slsmvs = Write {
	writeNext :: Maybe n,
	writeDstSet :: Dsc.Set.S sd sp sl,
	writeDstBinding :: Word32,
	writeDstArrayElement :: Word32,
	writeDescriptorType :: Dsc.Type,
	writeImageBufferInfoTexelBufferViews ::
		Either Word32 (ImageBufferInfoTexelBufferViews slsmvs) }

deriving instance (Show n, Show (HeteroVarList Dsc.BufferInfo slsmvs)) =>
	Show (Write n sd sp sl slsmvs)

data ImageBufferInfoTexelBufferViews slsmvs
	= ImageInfos [Dsc.M.ImageInfo]
	| BufferInfos (HeteroVarList Dsc.BufferInfo slsmvs)
	| TexelBufferViews [Buffer.View.V]

deriving instance Show (HeteroVarList Dsc.BufferInfo slsmvs) =>
	Show (ImageBufferInfoTexelBufferViews slsmvs)

data Write_ n sdspslslsmvs where
	Write_ :: Write n sd sp sl slsmvs -> Write_ n '(sd, sp, sl, slsmvs)

writeToCore :: (Pointable n, Dsc.BufferInfoListToMiddle slsmvs) =>
	Write n sd sp sl slsmvs -> ContT r IO C.Write
writeToCore Write {
	writeNext = mnxt,
	writeDstSet = Dsc.Set.S (Dsc.Set.M.S s),
	writeDstBinding = b,
	writeDstArrayElement = ae,
	writeDescriptorType = Dsc.Type tp,
	writeImageBufferInfoTexelBufferViews = mibitbvs
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	(dc, piis, pbis, ptbvs) <- case mibitbvs of
		Left c -> pure (c, NullPtr, NullPtr, NullPtr)
		Right (ImageInfos ((Dsc.M.imageInfoToCore <$>) -> ciis)) -> do
			(iic, p) <- allocaAndPokeArray ciis
			pure (fromIntegral iic, p, NullPtr, NullPtr)
		Right (BufferInfos (Dsc.bufferInfoListToCore -> cbis)) -> do
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

class WriteListToCore n sdspslslsmvs where
	writeListToCore ::
		HeteroVarList (Write_ n) sdspslslsmvs -> ContT r IO [C.Write]

instance WriteListToCore n '[] where writeListToCore HVNil = pure []

instance (Pointable n, Dsc.BufferInfoListToMiddle slsmvs, WriteListToCore n sdspslslsmvs) =>
	WriteListToCore n ('(sd, sp, sl, slsmvs) ': sdspslslsmvs) where
	writeListToCore (Write_ w :...: ws) = (:)
		<$> writeToCore w
		<*> writeListToCore ws

updateSs :: (WriteListToCore n sdspslslsmvs, Pointable n') =>
	Device.D sd -> HeteroVarList (Write_ n) sdspslslsmvs -> [Dsc.Set.M.Copy n'] -> IO ()
updateSs (Device.D (Device.M.D dvc)) ws cs = ($ pure) $ runContT do
	ws' <- writeListToCore ws
	(wc, pws) <- allocaAndPokeArray ws'
	(cc, pcs) <- allocaAndPokeArray =<< Dsc.Set.M.copyToCore `mapM` cs
	lift $ C.updateSs dvc (fromIntegral wc) pws (fromIntegral cc) pcs
