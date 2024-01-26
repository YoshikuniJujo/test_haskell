{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.RenderPass.Middle.Internal where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Control.Arrow
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.List qualified as TL
import qualified Data.HeteroParList as HeteroParList

import Gpu.Vulkan.Middle.Internal
import Gpu.Vulkan.Exception.Middle.Internal
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.RenderPass.Enum

import Gpu.Vulkan.Misc.Middle.Internal

import Gpu.Vulkan.AllocationCallbacks.Middle.Internal
	qualified as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Middle.Types as Device
import qualified Gpu.Vulkan.Attachment.Middle.Internal as Attachment
import qualified Gpu.Vulkan.Subpass.Middle.Internal as Subpass
import qualified Gpu.Vulkan.Framebuffer.Middle.Internal as Framebuffer
import qualified Gpu.Vulkan.RenderPass.Core as C

data CreateInfo mn = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: CreateFlags,
	createInfoAttachments :: [Attachment.Description],
	createInfoSubpasses :: [Subpass.Description],
	createInfoDependencies :: [Subpass.Dependency] }

deriving instance Show (TMaybe.M mn) => Show (CreateInfo mn)

createInfoToCore :: WithPoked (TMaybe.M mn) =>
	CreateInfo mn -> (Ptr C.CreateInfo -> IO r) -> IO ()
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoAttachments =
		(length &&& (Attachment.descriptionToCore <$>)) -> (ac, as),
	createInfoSubpasses = (length &&& id) -> (sc, ss),
	createInfoDependencies =
		(length &&& (Subpass.dependencyToCore <$>)) -> (dc, ds) } f =
	withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	allocaArray ac \pas ->
	pokeArray pas as >>
	(Subpass.descriptionToCore `mapContM` ss) \css ->
	allocaArray sc \pss ->
	pokeArray pss css >>
	allocaArray dc \pds ->
	pokeArray pds ds >>
	let ci = C.CreateInfo {
		C.createInfoSType = (),
		C.createInfoPNext = pnxt',
		C.createInfoFlags = flgs,
		C.createInfoAttachmentCount = fromIntegral ac,
		C.createInfoPAttachments = pas,
		C.createInfoSubpassCount = fromIntegral sc,
		C.createInfoPSubpasses = pss,
		C.createInfoDependencyCount = fromIntegral dc,
		C.createInfoPDependencies = pds } in
	withPoked ci f

newtype R = R C.R deriving Show

create :: WithPoked (TMaybe.M mn) =>
	Device.D -> CreateInfo mn -> TPMaybe.M AllocationCallbacks.A mc -> IO R
create (Device.D dvc) ci mac = R <$>
	alloca \pr -> do
		createInfoToCore ci \pci ->
			AllocationCallbacks.mToCore mac \pac -> do
				r <- C.create dvc pci pac pr
				throwUnlessSuccess $ Result r
		peek pr

destroy :: Device.D -> R -> TPMaybe.M AllocationCallbacks.A md -> IO ()
destroy (Device.D dvc) (R r) mac =
	AllocationCallbacks.mToCore mac $ C.destroy dvc r

data BeginInfo mn cts = BeginInfo {
	beginInfoNext :: TMaybe.M mn,
	beginInfoRenderPass :: R,
	beginInfoFramebuffer :: Framebuffer.F,
	beginInfoRenderArea :: Rect2d,
	beginInfoClearValues :: HeteroParList.PL ClearValue cts }

beginInfoToCore :: forall mn cts a . (WithPoked (TMaybe.M mn), ClearValueListToCore cts) =>
	BeginInfo mn cts -> (Ptr C.BeginInfo -> IO a) -> IO ()
beginInfoToCore BeginInfo {
	beginInfoNext = mnxt,
	beginInfoRenderPass = R rp,
	beginInfoFramebuffer = fb,
	beginInfoRenderArea = ra,
	beginInfoClearValues = const (TL.length @_ @cts) &&& id -> (cvc, cvs)
	} f = withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
		clearValueListToCore cvs \pcvl ->
		clearValueListToArray pcvl \pcva -> do
		fb' <- Framebuffer.fToCore fb
		let	ci = C.BeginInfo {
				C.beginInfoSType = (),
				C.beginInfoPNext = pnxt',
				C.beginInfoRenderPass = rp,
				C.beginInfoFramebuffer = fb',
				C.beginInfoRenderArea = ra,
				C.beginInfoClearValueCount = cvc,
				C.beginInfoPClearValues = pcva }
		withPoked ci f
