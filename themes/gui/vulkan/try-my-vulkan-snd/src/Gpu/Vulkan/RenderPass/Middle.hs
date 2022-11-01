{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.RenderPass.Middle where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Pointable
import Control.Arrow
import Control.Monad.Cont
import Data.HeteroList hiding (length)

import Gpu.Vulkan.Middle.Internal
import Gpu.Vulkan.Core (Rect2d)
import Gpu.Vulkan.Exception
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.RenderPass.Enum

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import {-# SOURCE #-} qualified Gpu.Vulkan.Device.Middle.Internal as Device
import qualified Gpu.Vulkan.Attachment as Attachment
import qualified Gpu.Vulkan.Subpass as Subpass
import qualified Gpu.Vulkan.Framebuffer.Middle.Internal as Framebuffer
import qualified Gpu.Vulkan.RenderPass.Core as C

data CreateInfoNew n fmts = CreateInfoNew {
	createInfoNextNew :: Maybe n,
	createInfoFlagsNew :: CreateFlags,
	createInfoAttachmentsNew ::
		HeteroVarList Attachment.DescriptionNew fmts,
	createInfoSubpassesNew :: [Subpass.Description],
	createInfoDependenciesNew :: [Subpass.Dependency] }

createInfoToCoreNew :: (Pointable n, Attachment.DescriptionsToCoreNew fmts) =>
	CreateInfoNew n fmts -> ContT r IO (Ptr C.CreateInfo)
createInfoToCoreNew CreateInfoNew {
	createInfoNextNew = mnxt,
	createInfoFlagsNew = CreateFlagBits flgs,
	createInfoAttachmentsNew = as_,
	createInfoSubpassesNew = (length &&& id) -> (sc, ss),
	createInfoDependenciesNew =
		(length &&& (Subpass.dependencyToCore <$>)) -> (dc, ds) } = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	pas <- ContT $ allocaArray ac
	lift $ pokeArray pas as
	css <- Subpass.descriptionToCore `mapM` ss
	pss <- ContT $ allocaArray sc
	lift $ pokeArray pss css
	pds <- ContT $ allocaArray dc
	lift $ pokeArray pds ds
	let	C.CreateInfo_ fCreateInfo = C.CreateInfo {
			C.createInfoSType = (),
			C.createInfoPNext = pnxt,
			C.createInfoFlags = flgs,
			C.createInfoAttachmentCount = fromIntegral ac,
			C.createInfoPAttachments = pas,
			C.createInfoSubpassCount = fromIntegral sc,
			C.createInfoPSubpasses = pss,
			C.createInfoDependencyCount = fromIntegral dc,
			C.createInfoPDependencies = pds }
	ContT $ withForeignPtr fCreateInfo
	where
	ac = length as
	as = Attachment.descriptionsToCoreNew as_

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoAttachments :: [Attachment.Description],
	createInfoSubpasses :: [Subpass.Description],
	createInfoDependencies :: [Subpass.Dependency] }
	deriving Show

createInfoToCore :: Pointable n => CreateInfo n -> ContT r IO (Ptr C.CreateInfo)
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoAttachments =
		(length &&& (Attachment.descriptionToCore <$>)) -> (ac, as),
	createInfoSubpasses = (length &&& id) -> (sc, ss),
	createInfoDependencies =
		(length &&& (Subpass.dependencyToCore <$>)) -> (dc, ds) } = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	pas <- ContT $ allocaArray ac
	lift $ pokeArray pas as
	css <- Subpass.descriptionToCore `mapM` ss
	pss <- ContT $ allocaArray sc
	lift $ pokeArray pss css
	pds <- ContT $ allocaArray dc
	lift $ pokeArray pds ds
	let	C.CreateInfo_ fCreateInfo = C.CreateInfo {
			C.createInfoSType = (),
			C.createInfoPNext = pnxt,
			C.createInfoFlags = flgs,
			C.createInfoAttachmentCount = fromIntegral ac,
			C.createInfoPAttachments = pas,
			C.createInfoSubpassCount = fromIntegral sc,
			C.createInfoPSubpasses = pss,
			C.createInfoDependencyCount = fromIntegral dc,
			C.createInfoPDependencies = pds }
	ContT $ withForeignPtr fCreateInfo

newtype R = R C.R deriving Show

createNew ::
	(Pointable n, Pointable c, Attachment.DescriptionsToCoreNew fmts) =>
	Device.D ->
	CreateInfoNew n fmts -> Maybe (AllocationCallbacks.A c) -> IO R
createNew (Device.D dvc) ci mac = ($ pure) . runContT $ R <$> do
	pci <- createInfoToCoreNew ci
	pac <- AllocationCallbacks.maybeToCore mac
	pr <- ContT alloca
	lift do	r <- C.create dvc pci pac pr
		throwUnlessSuccess $ Result r
		peek pr

create :: (Pointable n, Pointable n') =>
	Device.D -> CreateInfo n -> Maybe (AllocationCallbacks.A n') -> IO R
create (Device.D dvc) ci mac = ($ pure) . runContT $ R <$> do
	pci <- createInfoToCore ci
	pac <- AllocationCallbacks.maybeToCore mac
	pr <- ContT alloca
	lift do	r <- C.create dvc pci pac pr
		throwUnlessSuccess $ Result r
		peek pr

destroy :: Pointable n => Device.D -> R -> Maybe (AllocationCallbacks.A n) -> IO ()
destroy (Device.D dvc) (R r) mac = ($ pure) $ runContT do
	pac <- AllocationCallbacks.maybeToCore mac
	lift $ C.destroy dvc r pac

data BeginInfo n cts = BeginInfo {
	beginInfoNext :: Maybe n,
	beginInfoRenderPass :: R,
	beginInfoFramebuffer :: Framebuffer.F,
	beginInfoRenderArea :: Rect2d,
	beginInfoClearValues :: HeteroVarList ClearValue cts }

beginInfoToCore :: (Pointable n, ClearValuesToCore cts) =>
	BeginInfo n cts -> ContT r IO (Ptr C.BeginInfo)
beginInfoToCore BeginInfo {
	beginInfoNext = mnxt,
	beginInfoRenderPass = R rp,
	beginInfoFramebuffer = fb,
	beginInfoRenderArea = ra,
	beginInfoClearValues = heteroVarListLength &&& id -> (cvc, cvs)
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	pcvl <- clearValuesToCore cvs
	pcva <- clearValueListToArray pcvl
	fb' <- lift $ Framebuffer.fToCore fb
	let	C.BeginInfo_ fBeginInfo = C.BeginInfo {
			C.beginInfoSType = (),
			C.beginInfoPNext = pnxt,
			C.beginInfoRenderPass = rp,
			C.beginInfoFramebuffer = fb',
			C.beginInfoRenderArea = ra,
			C.beginInfoClearValueCount = fromIntegral cvc,
			C.beginInfoPClearValues = pcva }
	ContT $ withForeignPtr fBeginInfo
