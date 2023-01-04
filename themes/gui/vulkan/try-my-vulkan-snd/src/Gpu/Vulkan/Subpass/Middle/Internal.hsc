{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Subpass.Middle.Internal where

import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.C.Enum
import Foreign.Pointable
import Control.Arrow
import Control.Monad.Cont
import Data.Word

import Gpu.Vulkan.Enum
import Gpu.Vulkan.Subpass.Enum

import qualified Gpu.Vulkan.Pipeline.Enum as Pipeline
import qualified Gpu.Vulkan.Attachment as Attachment
import qualified Gpu.Vulkan.Subpass.Core as C

#include <vulkan/vulkan.h>

data Description = Description {
	descriptionFlags :: DescriptionFlags,
	descriptionPipelineBindPoint :: Pipeline.BindPoint,
	descriptionInputAttachments :: [Attachment.Reference],
	descriptionColorAndResolveAttachments :: Either
		[Attachment.Reference]
		[(Attachment.Reference, Attachment.Reference)],
	descriptionDepthStencilAttachment :: Maybe Attachment.Reference,
	descriptionPreserveAttachments :: [Word32] }
	deriving Show

descriptionToCore :: Description -> ContT r IO C.Description
descriptionToCore Description {
	descriptionFlags = DescriptionFlagBits flgs,
	descriptionPipelineBindPoint = Pipeline.BindPoint bp,
	descriptionInputAttachments =
		length &&& (Attachment.referenceToCore <$>) -> (iac, ias),
	descriptionColorAndResolveAttachments =
		(length &&& id)
			. either (, Nothing) ((Just `second`) . unzip) ->
		(cac, (	(Attachment.referenceToCore <$>) -> cas,
			((Attachment.referenceToCore <$>) <$>) -> mras)),
	descriptionDepthStencilAttachment =
		(Attachment.referenceToCore <$>) -> mdsa,
	descriptionPreserveAttachments = length &&& id -> (pac, pas) } = do
	pias <- ContT $ allocaArray iac
	lift $ pokeArray pias ias
	pcas <- ContT $ allocaArray cac
	lift $ pokeArray pcas cas
	pras <- case mras of
		Nothing -> pure NullPtr
		Just ras -> ContT (allocaArray cac) >>= \p ->
			p <$ lift (pokeArray p ras)
	pdsa <- case mdsa of
		Nothing -> pure NullPtr
		Just dsa -> ContT alloca >>= \p -> p <$ lift (poke p dsa)
	ppas <- ContT $ allocaArray pac
	lift $ pokeArray ppas pas
	pure C.Description {
		C.descriptionFlags = flgs,
		C.descriptionPipelineBindPoint = bp,
		C.descriptionInputAttachmentCount = fromIntegral iac,
		C.descriptionPInputAttachments = pias,
		C.descriptionColorAttachmentCount = fromIntegral cac,
		C.descriptionPColorAttachments = pcas,
		C.descriptionPResolveAttachments = pras,
		C.descriptionPDepthStencilAttachment = pdsa,
		C.descriptionPreserveAttachmentCount = fromIntegral pac,
		C.descriptionPPreserveAttachments = ppas }

enum "S" ''#{type uint32_t} [''Show, ''Storable]
	[("SExternal", #{const VK_SUBPASS_EXTERNAL})]

data Dependency = Dependency {
	dependencySrcSubpass :: S,
	dependencyDstSubpass :: S,
	dependencySrcStageMask :: Pipeline.StageFlags,
	dependencyDstStageMask :: Pipeline.StageFlags,
	dependencySrcAccessMask :: AccessFlags,
	dependencyDstAccessMask :: AccessFlags,
	dependencyDependencyFlags :: DependencyFlags }
	deriving Show

dependencyToCore :: Dependency -> C.Dependency
dependencyToCore Dependency {
	dependencySrcSubpass = S ss,
	dependencyDstSubpass = S ds,
	dependencySrcStageMask = Pipeline.StageFlagBits ssm,
	dependencyDstStageMask = Pipeline.StageFlagBits dsm,
	dependencySrcAccessMask = AccessFlagBits sam,
	dependencyDstAccessMask = AccessFlagBits dam,
	dependencyDependencyFlags = DependencyFlagBits flgs } = C.Dependency {
		C.dependencySrcSubpass = ss,
		C.dependencyDstSubpass = ds,
		C.dependencySrcStageMask = ssm,
		C.dependencyDstStageMask = dsm,
		C.dependencySrcAccessMask = sam,
		C.dependencyDstAccessMask = dam,
		C.dependencyDependencyFlags = flgs }
