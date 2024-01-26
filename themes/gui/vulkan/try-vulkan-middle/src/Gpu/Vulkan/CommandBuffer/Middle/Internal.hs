{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUaGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.CommandBuffer.Middle.Internal (
	C(..),
	AllocateInfo(..), allocateCs, freeCs,

	BeginInfo(..), InheritanceInfo(..), begin, end, reset
	) where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable.PeekPoke (
	WithPoked, withPoked, withPoked', withPtrS, pattern NullPtr )
import Control.Arrow
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.Default
import Data.IORef
import Data.Word

import Gpu.Vulkan.Enum
import Gpu.Vulkan.Exception.Middle.Internal
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.CommandBuffer.Enum
import Gpu.Vulkan.Base.Middle.Internal

import qualified Gpu.Vulkan.Device.Middle.Types as Device
import qualified Gpu.Vulkan.RenderPass.Middle.Internal as RenderPass
import qualified Gpu.Vulkan.Framebuffer.Middle.Internal as Framebuffer
import qualified Gpu.Vulkan.CommandPool.Middle.Internal as CommandPool
import qualified Gpu.Vulkan.CommandBuffer.Core as C
import qualified Gpu.Vulkan.Pipeline.Core as Pipeline.C

data AllocateInfo mn = AllocateInfo {
	allocateInfoNext :: TMaybe.M mn,
	allocateInfoCommandPool :: CommandPool.C,
	allocateInfoLevel :: Level,
	allocateInfoCommandBufferCount :: Word32 }

deriving instance Show (TMaybe.M mn) => Show (AllocateInfo mn)

allocateInfoToCore :: WithPoked (TMaybe.M mn) =>
	AllocateInfo mn -> (Ptr C.AllocateInfo -> IO a) -> IO ()
allocateInfoToCore AllocateInfo {
	allocateInfoNext = mnxt,
	allocateInfoCommandPool = CommandPool.C cp,
	allocateInfoLevel = Level lvl,
	allocateInfoCommandBufferCount = cbc
	} f = withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	withPoked C.AllocateInfo {
			C.allocateInfoSType = (),
			C.allocateInfoPNext = pnxt',
			C.allocateInfoCommandPool = cp,
			C.allocateInfoLevel = lvl,
			C.allocateInfoCommandBufferCount = cbc } f

data C = C {
	cPipeline :: IORef Pipeline.C.P,
	unC :: C.C }

newC :: C.C -> IO C
newC c = C <$> newIORef nullPtr <*> pure c

allocateCs :: WithPoked (TMaybe.M mn) => Device.D -> AllocateInfo mn -> IO [C]
allocateCs (Device.D dvc) ai =  mapM newC =<<
	allocaArray cbc \pc -> do
	allocateInfoToCore ai \pai -> do
		r <- C.allocateCs dvc pai pc
		throwUnlessSuccess $ Result r
	peekArray cbc pc
	where cbc = fromIntegral $ allocateInfoCommandBufferCount ai

data BeginInfo mn ii = BeginInfo {
	beginInfoNext :: TMaybe.M mn,
	beginInfoFlags :: UsageFlags,
	beginInfoInheritanceInfo :: Maybe (InheritanceInfo ii) }

instance Default (BeginInfo 'Nothing ii) where
	def = BeginInfo {
		beginInfoNext = TMaybe.N,
		beginInfoFlags = UsageFlagsZero,
		beginInfoInheritanceInfo = Nothing }

beginInfoToCore :: (WithPoked (TMaybe.M mn), WithPoked (TMaybe.M ii)) =>
	BeginInfo mn ii -> (Ptr C.BeginInfo -> IO a) -> IO ()
beginInfoToCore BeginInfo {
	beginInfoNext = mnxt,
	beginInfoFlags = UsageFlagBits flgs,
	beginInfoInheritanceInfo = mii } f =
	withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	maybe ($ NullPtr) inheritanceInfoToCore mii \pii ->
	() <$ withPoked C.BeginInfo {
			C.beginInfoSType = (),
			C.beginInfoPNext = pnxt',
			C.beginInfoFlags = flgs,
			C.beginInfoPInheritanceInfo = pii } f

data InheritanceInfo mn = InheritanceInfo {
	inheritanceInfoNext :: TMaybe.M mn,
	inheritanceInfoRenderPass :: RenderPass.R,
	inheritanceInfoSubpass :: Word32,
	inheritanceInfoFramebuffer :: Framebuffer.F,
	inheritanceInfoOcclusionQueryEnable :: Bool,
	inheritanceInfoQueryFlags :: QueryControlFlags,
	inheritanceInfoPipelineStatistics :: QueryPipelineStatisticFlags }

inheritanceInfoToCore :: WithPoked (TMaybe.M mn) =>
	InheritanceInfo mn -> (Ptr C.InheritanceInfo -> IO a) -> IO ()
inheritanceInfoToCore InheritanceInfo {
	inheritanceInfoNext = mnxt,
	inheritanceInfoRenderPass = RenderPass.R rp,
	inheritanceInfoSubpass = sp,
	inheritanceInfoFramebuffer = fb,
	inheritanceInfoOcclusionQueryEnable = oqe,
	inheritanceInfoQueryFlags = QueryControlFlagBits qf,
	inheritanceInfoPipelineStatistics = QueryPipelineStatisticFlagBits ps
	} f = withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	Framebuffer.fToCore fb >>= \fb' ->
	withPoked C.InheritanceInfo {
			C.inheritanceInfoSType = (),
			C.inheritanceInfoPNext = pnxt',
			C.inheritanceInfoRenderPass = rp,
			C.inheritanceInfoSubpass = sp,
			C.inheritanceInfoFramebuffer = fb',
			C.inheritanceInfoOcclusionQueryEnable = boolToBool32 oqe,
			C.inheritanceInfoQueryFlags = qf,
			C.inheritanceInfoPipelineStatistics = ps } f

begin :: (WithPoked (TMaybe.M mn), WithPoked (TMaybe.M ii)) => C -> BeginInfo mn ii -> IO ()
begin (C _ c) bi =
	beginInfoToCore bi \pbi -> throwUnlessSuccess . Result =<< C.begin c pbi

end :: C -> IO ()
end (C rppl c) = throwUnlessSuccess . Result =<< do
	writeIORef rppl nullPtr
	C.end c

reset :: C -> ResetFlags -> IO ()
reset (C _ c) (ResetFlagBits fs) = throwUnlessSuccess . Result =<< C.reset c fs

freeCs :: Device.D -> CommandPool.C -> [C] -> IO ()
freeCs (Device.D dvc) (CommandPool.C cp)
	(length &&& ((\(C _ cb) -> cb) <$>) -> (cc, cs)) = allocaArray cc \pcs -> do
		pokeArray pcs cs
		C.freeCs dvc cp (fromIntegral cc) pcs
