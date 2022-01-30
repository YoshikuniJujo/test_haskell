{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.RenderPass where

import Foreign.Marshal
import Foreign.Storable
import Control.Monad.Cont
import Data.Word

import Vulkan.Base
import Vulkan.SubpassDescriptionFlagBits
import Vulkan.PipelineBindPoint

import qualified Vulkan.RenderPass.Internal as I

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
