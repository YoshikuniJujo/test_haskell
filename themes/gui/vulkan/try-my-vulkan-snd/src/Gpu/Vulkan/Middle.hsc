{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Middle where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Storable
import Foreign.Pointable
import Control.Arrow
import Control.Monad.Cont
import Data.HeteroList hiding (length)
import Data.Word
import Data.Color.Internal

import qualified Data.Text as T

import Gpu.Vulkan.Base
import Gpu.Vulkan.Enum

import qualified Gpu.Vulkan.Core as C

import qualified Gpu.Vulkan.Pipeline.Enum as Pipeline
import {-# SOURCE #-} qualified Gpu.Vulkan.Semaphore.Middle as Semaphore
import {-# SOURCE #-} qualified
	Gpu.Vulkan.CommandBuffer.Middle.Internal as CommandBuffer

#include <vulkan/vulkan.h>

data ApplicationInfo a = ApplicationInfo {
	applicationInfoNext :: Maybe a,
	applicationInfoApplicationName :: T.Text,
	applicationInfoApplicationVersion :: ApiVersion,
	applicationInfoEngineName :: T.Text,
	applicationInfoEngineVersion :: ApiVersion,
	applicationInfoApiVersion :: ApiVersion }
	deriving Show

newtype ApiVersion = ApiVersion C.ApiVersion deriving (Show, Eq, Ord, Storable)

apiVersion_1_0 :: ApiVersion
apiVersion_1_0 = ApiVersion C.apiVersion_1_0

type Variant = Word8	-- 0 <= variant < 8
type Major = Word8	-- 0 <= major < 127
type Minor = Word16	-- 0 <= minor < 1023
type Patch = Word16	-- 0 <= patch < 4095

makeApiVersion :: Variant -> Major -> Minor -> Patch -> ApiVersion
makeApiVersion v mj mn p = ApiVersion $ C.makeApiVersion v mj mn p

applicationInfoToCore :: Pointable a =>
	ApplicationInfo a -> ContT r IO (Ptr C.ApplicationInfo)
applicationInfoToCore ApplicationInfo {
	applicationInfoNext = mnxt,
	applicationInfoApplicationName = anm,
	applicationInfoApplicationVersion = (\(ApiVersion v) -> v) -> appv,
	applicationInfoEngineName = enm,
	applicationInfoEngineVersion = (\(ApiVersion v) -> v) -> engv,
	applicationInfoApiVersion = (\(ApiVersion v) -> v) -> apiv
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	canm <- textToCString anm
	cenm <- textToCString enm
	let	C.ApplicationInfo_ fApplicationInfo = C.ApplicationInfo {
			C.applicationInfoSType = (),
			C.applicationInfoPNext = pnxt,
			C.applicationInfoPApplicationName = canm,
			C.applicationInfoApplicationVersion = appv,
			C.applicationInfoPEngineName = cenm,
			C.applicationInfoEngineVersion = engv,
			C.applicationInfoApiVersion = apiv }
	ContT $ withForeignPtr fApplicationInfo

newtype ObjectHandle = ObjectHandle #{type uint64_t} deriving Show

data ExtensionProperties = ExtensionProperties {
	extensionPropertiesExtensionName :: T.Text,
	extensionPropertiesSpecVersion :: ApiVersion }
	deriving Show

extensionPropertiesFromCore :: C.ExtensionProperties -> ExtensionProperties
extensionPropertiesFromCore C.ExtensionProperties {
	C.extensionPropertiesExtensionName = en,
	C.extensionPropertiesSpecVersion = sv } = ExtensionProperties {
		extensionPropertiesExtensionName = en,
		extensionPropertiesSpecVersion = ApiVersion sv }

data LayerProperties = LayerProperties {
	layerPropertiesLayerName :: T.Text,
	layerPropertiesSpecVersion :: ApiVersion,
	layerPropertiesImplementationVersion :: ApiVersion,
	layerPropertiesDescription :: T.Text }
	deriving Show

layerPropertiesFromCore :: C.LayerProperties -> LayerProperties
layerPropertiesFromCore C.LayerProperties {
	C.layerPropertiesLayerName = ln,
	C.layerPropertiesSpecVersion = sv,
	C.layerPropertiesImplementationVersion = iv,
	C.layerPropertiesDescription = dsc } = LayerProperties {
	layerPropertiesLayerName = ln,
	layerPropertiesSpecVersion = ApiVersion sv,
	layerPropertiesImplementationVersion = ApiVersion iv,
	layerPropertiesDescription = dsc }

data StencilOpState = StencilOpState {
	stencilOpStateFailOp :: StencilOp,
	stencilOpStatePassOp :: StencilOp,
	stencilOpStateDepthFailOp :: StencilOp,
	stencilOpStateCompareOp :: CompareOp,
	stencilOpStateCompareMask :: Word32,
	stencilOpStateWriteMask :: Word32,
	stencilOpStateReference :: Word32 }
	deriving Show

stencilOpStateZero :: StencilOpState
stencilOpStateZero = StencilOpState {
	stencilOpStateFailOp = StencilOpKeep,
	stencilOpStatePassOp = StencilOpKeep,
	stencilOpStateDepthFailOp = StencilOpKeep,
	stencilOpStateCompareOp = CompareOpNever,
	stencilOpStateCompareMask = 0,
	stencilOpStateWriteMask = 0,
	stencilOpStateReference = 0 }

stencilOpStateToCore :: StencilOpState -> C.StencilOpState
stencilOpStateToCore StencilOpState {
	stencilOpStateFailOp = StencilOp fo,
	stencilOpStatePassOp = StencilOp po,
	stencilOpStateDepthFailOp = StencilOp dfo,
	stencilOpStateCompareOp = CompareOp co,
	stencilOpStateCompareMask = cm,
	stencilOpStateWriteMask = wm,
	stencilOpStateReference = rf } = C.StencilOpState {
		C.stencilOpStateFailOp = fo,
		C.stencilOpStatePassOp = po,
		C.stencilOpStateDepthFailOp = dfo,
		C.stencilOpStateCompareOp = co,
		C.stencilOpStateCompareMask = cm,
		C.stencilOpStateWriteMask = wm,
		C.stencilOpStateReference = rf }

data ClearValue (ct :: ClearType) where
	ClearValueDepthStencil ::
		C.ClearDepthStencilValue -> ClearValue 'ClearTypeDepthStencil
	ClearValueColor :: Rgba Float -> ClearValue ('ClearTypeColor cct)

class ClearColorValueToCore (cct :: ClearColorType) where
	clearColorValueToCore ::
		ClearValue ('ClearTypeColor cct) -> ContT r IO (Ptr C.ClearColorValueTag)

instance ClearColorValueToCore 'ClearColorTypeFloat32 where
	clearColorValueToCore (ClearValueColor (RgbaDouble r g b a)) = do
		prgba <- ContT $ allocaArray 4
		lift $ pokeArray prgba [r, g, b, a]
		pure $ C.clearColorValueFromFloats prgba

instance ClearColorValueToCore 'ClearColorTypeInt32 where
	clearColorValueToCore (ClearValueColor (RgbaInt32 r g b a)) = do
		prgba <- ContT $ allocaArray 4
		lift $ pokeArray prgba [r, g, b, a]
		pure $ C.clearColorValueFromInts prgba

instance ClearColorValueToCore 'ClearColorTypeUint32 where
	clearColorValueToCore (ClearValueColor (RgbaWord32 r g b a)) = do
		prgba <- ContT $ allocaArray 4
		lift $ pokeArray prgba [r, g, b, a]
		pure $ C.clearColorValueFromUints prgba

deriving instance Show (ClearValue ct)

data ClearType = ClearTypeColor ClearColorType | ClearTypeDepthStencil
	deriving Show

data ClearColorType
	= ClearColorTypeFloat32 | ClearColorTypeInt32 | ClearColorTypeUint32
	deriving Show

class ClearValueToCore (ct :: ClearType) where
	clearValueToCore :: ClearValue ct -> ContT r IO (Ptr C.ClearValueTag)

instance ClearValueToCore 'ClearTypeDepthStencil where
	clearValueToCore (ClearValueDepthStencil cdsv) =
		C.clearValueFromClearDepthStencilValue cdsv

instance ClearColorValueToCore cct =>
	ClearValueToCore ('ClearTypeColor cct) where
	clearValueToCore cv@(ClearValueColor _) =
		C.clearValueFromClearColorValue <$> clearColorValueToCore cv

class ClearValuesToCore (cts :: [ClearType]) where
	clearValuesToCore :: HeteroVarList ClearValue cts -> ContT r IO [Ptr C.ClearValueTag]

instance ClearValuesToCore '[] where clearValuesToCore HVNil = pure []

instance (ClearValueToCore ct, ClearValuesToCore cts) =>
	ClearValuesToCore (ct ': cts) where
	clearValuesToCore (cv :...: cvs) = (:)
		<$> clearValueToCore cv
		<*> clearValuesToCore cvs

clearValueListToArray :: [Ptr C.ClearValueTag] -> ContT r IO (Ptr C.ClearValueTag)
clearValueListToArray (length &&& id -> (pcvc, pcvl)) = do
	pcva <- allocaClearValueArray pcvc
	lift $ pokeClearValueArray pcva pcvl
	pure pcva

allocaClearValueArray :: Int -> ContT r IO (Ptr C.ClearValueTag)
allocaClearValueArray n = ContT $ allocaBytesAligned
	(alignedSize #{size VkClearValue} #{alignment VkClearValue} * n)
	#{alignment VkClearValue}

alignedSize :: Int -> Int -> Int
alignedSize sz al = (sz - 1) `div` al * al + 1

pokeClearValueArray :: Ptr C.ClearValueTag -> [Ptr C.ClearValueTag] -> IO ()
pokeClearValueArray p lst = zipWithM_ pokeClearValue (clearValueArrayPtrs p) lst

clearValueArrayPtrs :: Ptr C.ClearValueTag -> [Ptr C.ClearValueTag]
clearValueArrayPtrs = iterate (
	(`alignPtr` #{alignment VkClearValue})
		. (`plusPtr` #{size VkClearValue}) )

pokeClearValue :: Ptr C.ClearValueTag -> Ptr C.ClearValueTag -> IO ()
pokeClearValue dst src = copyBytes dst src #{size VkClearValue}

data SubmitInfoNew n vss = SubmitInfoNew {
	submitInfoNextNew :: Maybe n,
	submitInfoWaitSemaphoreDstStageMasksNew ::
		[(Semaphore.S, Pipeline.StageFlags)],
	submitInfoCommandBuffersNew :: HeteroVarList CommandBuffer.CC vss,
	submitInfoSignalSemaphoresNew :: [Semaphore.S] }

deriving instance (Show n, Show (HeteroVarList CommandBuffer.CC vss)) =>
	Show (SubmitInfoNew n vss)

data SubmitInfo n vs = SubmitInfo {
	submitInfoNext :: Maybe n,
	submitInfoWaitSemaphoreDstStageMasks ::
		[(Semaphore.S, Pipeline.StageFlags)],
	submitInfoCommandBuffers :: [CommandBuffer.CC vs],
	submitInfoSignalSemaphores :: [Semaphore.S] }

submitInfoToCoreNew :: Pointable n => SubmitInfoNew n vs -> ContT r IO C.SubmitInfo
submitInfoToCoreNew SubmitInfoNew {
	submitInfoNextNew = mnxt,
	submitInfoWaitSemaphoreDstStageMasksNew =
		length &&&
		(	(Semaphore.unS <$>) ***
			(Pipeline.unStageFlagBits <$>)) . unzip ->
		(wsc, (wss, wdsms)),
	submitInfoCommandBuffersNew = (length &&& id)
		. heteroVarListToList (CommandBuffer.unC . CommandBuffer.unCC) -> (cbc, cbs),
	submitInfoSignalSemaphoresNew =
		length &&& (Semaphore.unS <$>) -> (ssc, sss)
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	pwss <- ContT $ allocaArray wsc
	lift $ pokeArray pwss wss
	pwdsms <- ContT $ allocaArray wsc
	lift $ pokeArray pwdsms wdsms
	pcbs <- ContT $ allocaArray cbc
	lift $ pokeArray pcbs cbs
	psss <- ContT $ allocaArray ssc
	lift $ pokeArray psss sss
	pure C.SubmitInfo {
		C.submitInfoSType = (),
		C.submitInfoPNext = pnxt,
		C.submitInfoWaitSemaphoreCount = fromIntegral wsc,
		C.submitInfoPWaitSemaphores = pwss,
		C.submitInfoPWaitDstStageMask = pwdsms,
		C.submitInfoCommandBufferCount = fromIntegral cbc,
		C.submitInfoPCommandBuffers = pcbs,
		C.submitInfoSignalSemaphoreCount = fromIntegral ssc,
		C.submitInfoPSignalSemaphores = psss }

submitInfoToCore :: Pointable n => SubmitInfo n vs -> ContT r IO C.SubmitInfo
submitInfoToCore SubmitInfo {
	submitInfoNext = mnxt,
	submitInfoWaitSemaphoreDstStageMasks =
		length &&&
		(	(Semaphore.unS <$>) ***
			(Pipeline.unStageFlagBits <$>)) . unzip ->
		(wsc, (wss, wdsms)),
	submitInfoCommandBuffers =
		length &&& (CommandBuffer.unC . CommandBuffer.unCC <$>) -> (cbc, cbs),
	submitInfoSignalSemaphores =
		length &&& (Semaphore.unS <$>) -> (ssc, sss)
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	pwss <- ContT $ allocaArray wsc
	lift $ pokeArray pwss wss
	pwdsms <- ContT $ allocaArray wsc
	lift $ pokeArray pwdsms wdsms
	pcbs <- ContT $ allocaArray cbc
	lift $ pokeArray pcbs cbs
	psss <- ContT $ allocaArray ssc
	lift $ pokeArray psss sss
	pure C.SubmitInfo {
		C.submitInfoSType = (),
		C.submitInfoPNext = pnxt,
		C.submitInfoWaitSemaphoreCount = fromIntegral wsc,
		C.submitInfoPWaitSemaphores = pwss,
		C.submitInfoPWaitDstStageMask = pwdsms,
		C.submitInfoCommandBufferCount = fromIntegral cbc,
		C.submitInfoPCommandBuffers = pcbs,
		C.submitInfoSignalSemaphoreCount = fromIntegral ssc,
		C.submitInfoPSignalSemaphores = psss }
