{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.RenderPass where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import Control.Monad.Cont
import Data.Word

import Vulkan.Base
import Vulkan.Exception
import Vulkan.AllocationCallbacks
import Vulkan.Device
import Vulkan.SubpassDescriptionFlagBits
import Vulkan.PipelineBindPoint

import Vulkan.RenderPassCreateFlagBits

import qualified Vulkan.AllocationCallbacks.Internal as I
import qualified Vulkan.RenderPass.Internal as I

import Vulkan.Framebuffer (Framebuffer)
import Vulkan.Clear as Clear

#include <vulkan/vulkan.h>

data SubpassDescription = SubpassDescription {
	subpassDescriptionFlags :: SubpassDescriptionFlags,
	subpassDescriptionPipelineBindPoint :: PipelineBindPoint,
	subpassDescriptionInputAttachments :: [I.AttachmentReference],
	subpassDescriptionColorAndResolveAttachments :: Either
		[I.AttachmentReference]
		[(I.AttachmentReference, I.AttachmentReference)],
	subpassDescriptionDepthStencilAttachment :: Maybe I.AttachmentReference,
	subpassDescriptionPreserveAttachments :: [#{type uint32_t}] }
	deriving Show

subpassDescriptionToC ::
	SubpassDescription -> (I.SubpassDescription -> IO a) -> IO a
subpassDescriptionToC SubpassDescription {
	subpassDescriptionFlags = flgs,
	subpassDescriptionPipelineBindPoint = pbp,
	subpassDescriptionInputAttachments = ias,
	subpassDescriptionColorAndResolveAttachments = ecras,
	subpassDescriptionDepthStencilAttachment = msa,
	subpassDescriptionPreserveAttachments = pas } = runContT do
	let	iac = length ias
	pias <- ContT $ allocaArray iac
	lift $ pokeArray pias ias
	(cac, pcas, pras) <- case ecras of
		Left cas -> do
			let	cac = length cas
			pcas' <- ContT $ allocaArray cac
			lift $ pokeArray pcas' cas
			pure (cac, pcas', NullPtr)
		Right cras -> do
			let	cac = length cras
				(cas, ras) = unzip cras
			pcas' <- ContT $ allocaArray cac
			lift $ pokeArray pcas' cas
			pras' <- ContT $ allocaArray cac
			lift $ pokeArray pras' ras
			pure (cac, pcas', pras')
	psa <- case msa of
		Nothing -> pure NullPtr
		Just sa -> do
			psa' <- ContT alloca
			lift $ poke psa' sa
			pure psa'
	let	pac = length pas
	ppas <- ContT $ allocaArray pac
	lift $ pokeArray ppas pas
	pure I.SubpassDescription {
		I.subpassDescriptionFlags = flgs,
		I.subpassDescriptionPipelineBindPoint = pbp,
		I.subpassDescriptionInputAttachmentCount = fromIntegral iac,
		I.subpassDescriptionPInputAttachments = pias,
		I.subpassDescriptionColorAttachmentCount = fromIntegral cac,
		I.subpassDescriptionPColorAttachments = pcas,
		I.subpassDescriptionPResolveAttachments = pras,
		I.subpassDescriptionPDepthStencilAttachment = psa,
		I.subpassDescriptionPreserveAttachmentCount = fromIntegral pac,
		I.subpassDescriptionPPreserveAttachments = ppas
		}

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: RenderPassCreateFlags,
	createInfoAttachments :: [I.AttachmentDescription],
	createInfoSubpasses :: [SubpassDescription],
	createInfoDependencies :: [I.SubpassDependency] }
	deriving Show

createInfoToC :: Pointable n => CreateInfo n -> (I.CreateInfo -> IO a) -> IO a
createInfoToC CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoAttachments = as,
	createInfoSubpasses = ss,
	createInfoDependencies = ds } = runContT do
	(castPtr -> pnxt) <- ContT $ withMaybePointer mnxt
	let	ac = length as
	pas <- ContT $ allocaArray ac
	lift $ pokeArray pas as
	let	sc = length ss
	pss <- ContT $ allocaArray sc
	iss <- (ContT . subpassDescriptionToC) `mapM` ss
	lift $ pokeArray pss iss
	let	dc = length ds
	pds <- ContT $ allocaArray dc
	lift $ pokeArray pds ds
	pure I.CreateInfo {
		I.createInfoSType = (),
		I.createInfoPNext = pnxt,
		I.createInfoFlags = flgs,
		I.createInfoAttachmentCount = fromIntegral ac,
		I.createInfoPAttachments = pas,
		I.createInfoSubpassCount = fromIntegral sc,
		I.createInfoPSubpasses = pss,
		I.createInfoDependencyCount = fromIntegral dc,
		I.createInfoPDependencies = pds }

create :: (Pointable n, Pointable n') => Device ->
	CreateInfo n -> Maybe (AllocationCallbacks n') -> IO RenderPass
create dvc ci mac = ($ pure) $ runContT do
	I.CreateInfo_ fci <- ContT $ createInfoToC ci
	pci <- ContT $ withForeignPtr fci
	pac <- case mac of
		Nothing -> pure NullPtr
		Just ac -> ContT $ withAllocationCallbacksPtr ac
	prp <- ContT alloca
	lift do	r <- c_vkCreateRenderPass dvc pci pac prp
		throwUnlessSuccess r
		peek prp

foreign import ccall "vkCreateRenderPass" c_vkCreateRenderPass ::
	Device -> Ptr I.CreateInfo -> Ptr I.AllocationCallbacks ->
	Ptr RenderPass -> IO Result

destroy :: Pointable n =>
	Device -> RenderPass -> Maybe (AllocationCallbacks n) -> IO ()
destroy dvc rp mac = ($ pure) $ runContT do
	pac <- case mac of
		Nothing -> pure NullPtr
		Just ac -> ContT $ withAllocationCallbacksPtr ac
	lift $ c_vkDestroyRenderPass dvc rp pac

foreign import ccall "vkDestroyRenderPass" c_vkDestroyRenderPass ::
	Device -> RenderPass -> Ptr I.AllocationCallbacks -> IO ()

data BeginInfo n = BeginInfo {
	beginInfoNext :: Maybe n,
	beginInfoRenderPass :: RenderPass,
	beginInfoFramebuffer :: Framebuffer,
	beginInfoRenderArea :: Rect2d,
	beginInfoClearValues :: [Clear.Value] }
	deriving Show

beginInfoToC :: Pointable n => BeginInfo n -> ContT r IO I.BeginInfo
beginInfoToC BeginInfo {
	beginInfoNext = mnxt,
	beginInfoRenderPass = rp,
	beginInfoFramebuffer = fb,
	beginInfoRenderArea = ra,
	beginInfoClearValues = cvs } = do
	(castPtr -> pnxt) <- ContT $ withMaybePointer mnxt
	let	cvc = length cvs
	cvps <- valueToPtr `mapM` cvs
	pcvps <- ContT $ allocaArray cvc
	lift $ pokeArray pcvps cvps
	pure I.BeginInfo {
		I.beginInfoSType = (),
		I.beginInfoPNext = pnxt,
		I.beginInfoRenderPass = rp,
		I.beginInfoFramebuffer = fb,
		I.beginInfoRenderArea = ra,
		I.beginInfoClearValueCount = fromIntegral cvc,
		I.beginInfoPClearValues = pcvps }
