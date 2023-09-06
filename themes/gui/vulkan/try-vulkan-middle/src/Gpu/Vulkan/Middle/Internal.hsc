{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Middle.Internal (
	ApplicationInfo(..), applicationInfoToCore,
	ApiVersion(..), makeApiVersion, apiVersion_1_0, apiVersion_1_1,
	LayerProperties(..), layerPropertiesFromCore,
	ExtensionProperties(..), extensionPropertiesFromCore,
	StencilOpState(..), stencilOpStateToCore,
	ClearValue(..), ClearValueListToCore(..),
	ClearValueToCore, ClearColorValueToCore,
	clearValueListToArray,
	ClearType(..), ClearColorType(..),

	C.ClearDepthStencilValue, pattern C.ClearDepthStencilValue,
	C.clearDepthStencilValueDepth, C.clearDepthStencilValueStencil,

	SubmitInfo(..), SubmitInfoListToCore(..), submitInfoToCore,

	FormatProperties(..), formatPropertiesFromCore,

	C.Rect2d, pattern C.Rect2d, C.rect2dExtent, C.rect2dOffset,

	C.Offset2d, pattern C.Offset2d, C.offset2dX, C.offset2dY,
	C.Offset3d, pattern C.Offset3d, C.offset3dX, C.offset3dY, C.offset3dZ,

	C.Extent2d, pattern C.Extent2d, C.extent2dWidth, C.extent2dHeight,
	C.Extent3d,
	pattern C.Extent3d, C.extent3dWidth, C.extent3dHeight, C.extent3dDepth,

	C.Viewport, pattern C.Viewport,
	C.viewportX, C.viewportY, C.viewportWidth, C.viewportHeight,
	C.viewportMinDepth, C.viewportMaxDepth

	) where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Control.Arrow
import Control.Monad
import Data.Default
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.List qualified as TL
import Data.HeteroParList qualified as HeteroParList
import Data.HeteroParList (pattern (:**))
import Data.Word
import Data.Text.Foreign.Misc
import Data.Color.Internal

import qualified Data.Text as T

import Gpu.Vulkan.Enum

import qualified Gpu.Vulkan.Core as C

import qualified Gpu.Vulkan.Pipeline.Enum as Pipeline
import {-# SOURCE #-} qualified Gpu.Vulkan.Semaphore.Middle.Internal as Semaphore
import {-# SOURCE #-} qualified
	Gpu.Vulkan.CommandBuffer.Middle.Internal as CommandBuffer

#include <vulkan/vulkan.h>

data ApplicationInfo mn = ApplicationInfo {
	applicationInfoNext :: TMaybe.M mn,
	applicationInfoApplicationName :: T.Text,
	applicationInfoApplicationVersion :: ApiVersion,
	applicationInfoEngineName :: T.Text,
	applicationInfoEngineVersion :: ApiVersion,
	applicationInfoApiVersion :: ApiVersion }

deriving instance Show (TMaybe.M mn) => Show (ApplicationInfo mn)

newtype ApiVersion = ApiVersion C.ApiVersion deriving (Show, Eq, Ord, Storable)

apiVersion_1_0, apiVersion_1_1 :: ApiVersion
apiVersion_1_0 = ApiVersion C.apiVersion_1_0
apiVersion_1_1 = ApiVersion C.apiVersion_1_1

type Variant = Word8	-- 0 <= variant < 8
type Major = Word8	-- 0 <= major < 127
type Minor = Word16	-- 0 <= minor < 1023
type Patch = Word16	-- 0 <= patch < 4095

makeApiVersion :: Variant -> Major -> Minor -> Patch -> ApiVersion
makeApiVersion v mj mn p = ApiVersion $ C.makeApiVersion v mj mn p

applicationInfoToCore :: WithPoked (TMaybe.M mn) =>
	ApplicationInfo mn -> (Ptr C.ApplicationInfo -> IO a) -> IO ()
applicationInfoToCore ApplicationInfo {
	applicationInfoNext = mnxt,
	applicationInfoApplicationName = anm,
	applicationInfoApplicationVersion = (\(ApiVersion v) -> v) -> appv,
	applicationInfoEngineName = enm,
	applicationInfoEngineVersion = (\(ApiVersion v) -> v) -> engv,
	applicationInfoApiVersion = (\(ApiVersion v) -> v) -> apiv
	} f = withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	textToCString anm \canm -> textToCString enm \cenm ->
	let	appInfo = C.ApplicationInfo {
			C.applicationInfoSType = (),
			C.applicationInfoPNext = pnxt',
			C.applicationInfoPApplicationName = canm,
			C.applicationInfoApplicationVersion = appv,
			C.applicationInfoPEngineName = cenm,
			C.applicationInfoEngineVersion = engv,
			C.applicationInfoApiVersion = apiv } in
	withPoked appInfo f

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

instance Default StencilOpState where def = stencilOpStateZero

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
	ClearValueColor :: Rgba Float -> ClearValue ('ClearTypeColor cct)
	ClearValueDepthStencil ::
		C.ClearDepthStencilValue -> ClearValue 'ClearTypeDepthStencil

class ClearColorValueToCore (cct :: ClearColorType) where
	clearColorValueToCore ::
		ClearValue ('ClearTypeColor cct) ->
		(Ptr C.ClearColorValue -> IO a) -> IO a

instance ClearColorValueToCore 'ClearColorTypeFloat32 where
	clearColorValueToCore (ClearValueColor (RgbaDouble r g b a)) f =
		allocaArray 4 \prgba -> do
			pokeArray prgba [r, g, b, a]
			f $ C.clearColorValueFromFloats prgba

instance ClearColorValueToCore 'ClearColorTypeInt32 where
	clearColorValueToCore (ClearValueColor (RgbaInt32 r g b a)) f =
		allocaArray 4 \prgba -> do
			pokeArray prgba [r, g, b, a]
			f $ C.clearColorValueFromInts prgba

instance ClearColorValueToCore 'ClearColorTypeUint32 where
	clearColorValueToCore (ClearValueColor (RgbaWord32 r g b a)) f =
		allocaArray 4 \prgba -> do
			pokeArray prgba [r, g, b, a]
			f $ C.clearColorValueFromUints prgba

deriving instance Show (ClearValue ct)

data ClearType = ClearTypeColor ClearColorType | ClearTypeDepthStencil
	deriving Show

data ClearColorType
	= ClearColorTypeFloat32 | ClearColorTypeInt32 | ClearColorTypeUint32
	deriving Show

class ClearValueToCore (ct :: ClearType) where
	clearValueToCore :: ClearValue ct -> (Ptr C.ClearValue -> IO a) -> IO a

instance ClearValueToCore 'ClearTypeDepthStencil where
	clearValueToCore (ClearValueDepthStencil cdsv) =
		C.clearValueFromClearDepthStencilValue cdsv

instance ClearColorValueToCore cct =>
	ClearValueToCore ('ClearTypeColor cct) where
	clearValueToCore cv@(ClearValueColor _) f =
		clearColorValueToCore cv $ f . C.clearValueFromClearColorValue

class TL.Length cts => ClearValueListToCore (cts :: [ClearType]) where
	clearValueListToCore ::
		HeteroParList.PL ClearValue cts ->
		([Ptr C.ClearValue] -> IO a) -> IO a

instance ClearValueListToCore '[] where clearValueListToCore HeteroParList.Nil = ($ [])

instance (ClearValueToCore ct, ClearValueListToCore cts) =>
	ClearValueListToCore (ct ': cts) where
	clearValueListToCore (cv :** cvs) f =
		clearValueToCore cv \ccv ->
		clearValueListToCore cvs \ccvs -> f $ ccv : ccvs

clearValueListToArray :: [Ptr C.ClearValue] -> (Ptr C.ClearValue -> IO a) -> IO a
clearValueListToArray (length &&& id -> (pcvc, pcvl)) f =
	allocaClearValueArray pcvc \pcva -> do
		pokeClearValueArray pcva pcvl
		f pcva

allocaClearValueArray :: Int -> (Ptr C.ClearValue -> IO a) -> IO a
allocaClearValueArray n = allocaBytesAligned
	(alignedSize #{size VkClearValue} #{alignment VkClearValue} * n)
	#{alignment VkClearValue}

alignedSize :: Int -> Int -> Int
alignedSize sz al = (sz - 1) `div` al * al + 1

pokeClearValueArray :: Ptr C.ClearValue -> [Ptr C.ClearValue] -> IO ()
pokeClearValueArray p lst = zipWithM_ pokeClearValue (clearValueArrayPtrs p) lst

clearValueArrayPtrs :: Ptr C.ClearValue -> [Ptr C.ClearValue]
clearValueArrayPtrs = iterate (
	(`alignPtr` #{alignment VkClearValue})
		. (`plusPtr` #{size VkClearValue}) )

pokeClearValue :: Ptr C.ClearValue -> Ptr C.ClearValue -> IO ()
pokeClearValue dst src = copyBytes dst src #{size VkClearValue}

data SubmitInfo mn = SubmitInfo {
	submitInfoNext :: TMaybe.M mn,
	submitInfoWaitSemaphoreDstStageMasks ::
		[(Semaphore.S, Pipeline.StageFlags)],
	submitInfoCommandBuffers :: [CommandBuffer.C],
	submitInfoSignalSemaphores :: [Semaphore.S] }

class SubmitInfoListToCore ns where
	submitInfoListToCore :: HeteroParList.PL SubmitInfo ns ->
		([C.SubmitInfo] -> IO a) -> IO ()

instance SubmitInfoListToCore '[] where
	submitInfoListToCore HeteroParList.Nil f = () <$ f []

instance (WithPoked (TMaybe.M n), SubmitInfoListToCore ns) =>
	SubmitInfoListToCore (n ': ns) where
	submitInfoListToCore (ci :** cis) f = submitInfoToCore @n ci \cci ->
		submitInfoListToCore cis \ccis -> f $ cci : ccis

submitInfoToCore :: WithPoked (TMaybe.M mn) =>
	SubmitInfo mn -> (C.SubmitInfo -> IO a) -> IO ()
submitInfoToCore SubmitInfo {
	submitInfoNext = mnxt,
	submitInfoWaitSemaphoreDstStageMasks =
		length &&&
		(	(Semaphore.unS <$>) ***
			(Pipeline.unStageFlagBits <$>)) . unzip ->
		(wsc, (wss, wdsms)),
	submitInfoCommandBuffers = (length &&& id) . map CommandBuffer.unC -> (cbc, cbs),
	submitInfoSignalSemaphores =
		length &&& (Semaphore.unS <$>) -> (ssc, sss) } f =
	withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	allocaArray wsc \pwss ->
	pokeArray pwss wss >>
	allocaArray wsc \pwdsms ->
	pokeArray pwdsms wdsms >>
	allocaArray cbc \pcbs ->
	pokeArray pcbs cbs >>
	allocaArray ssc \psss ->
	pokeArray psss sss >>
	f C.SubmitInfo {
		C.submitInfoSType = (),
		C.submitInfoPNext = pnxt',
		C.submitInfoWaitSemaphoreCount = fromIntegral wsc,
		C.submitInfoPWaitSemaphores = pwss,
		C.submitInfoPWaitDstStageMask = pwdsms,
		C.submitInfoCommandBufferCount = fromIntegral cbc,
		C.submitInfoPCommandBuffers = pcbs,
		C.submitInfoSignalSemaphoreCount = fromIntegral ssc,
		C.submitInfoPSignalSemaphores = psss }

data FormatProperties = FormatProperties {
	formatPropertiesLinearTilingFeatures :: FormatFeatureFlags,
	formatPropertiesOptimalTilingFeatures :: FormatFeatureFlags,
	formatPropertiesBufferFeatures :: FormatFeatureFlags }
	deriving Show

formatPropertiesFromCore :: C.FormatProperties -> FormatProperties
formatPropertiesFromCore C.FormatProperties {
	C.formatPropertiesLinearTilingFeatures = ltfs,
	C.formatPropertiesOptimalTilingFeatures = otfs,
	C.formatPropertiesBufferFeatures = bfs
	} = FormatProperties {
		formatPropertiesLinearTilingFeatures =
			FormatFeatureFlagBits ltfs,
		formatPropertiesOptimalTilingFeatures =
			FormatFeatureFlagBits otfs,
		formatPropertiesBufferFeatures = FormatFeatureFlagBits bfs }
