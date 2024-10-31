{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Subpass.Middle.Internal where

import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Foreign.C.Enum
import Control.Arrow
import Data.Word

import Gpu.Vulkan.Enum
import Gpu.Vulkan.Subpass.Enum

import qualified Gpu.Vulkan.Pipeline.Enum as Pipeline
import qualified Gpu.Vulkan.Attachment.Middle.Internal as Attachment
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

descriptionToCore :: Description -> (C.Description -> IO r) -> IO r
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
	descriptionPreserveAttachments = length &&& id -> (pac, pas) } f =
	allocaArray iac \pias ->
	pokeArray pias ias >>
	allocaArray cac \pcas ->
	pokeArray pcas cas >>
	maybe ($ NullPtr)
		(\ras g -> allocaArray cac \p -> pokeArray p ras >> g p)
		mras \pras ->
	maybe ($ NullPtr)
		(\dsa g -> alloca \p -> poke p dsa >> g p) mdsa \pdsa ->
	allocaArray pac \ppas ->
	pokeArray ppas pas >>
	f C.Description {
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

enum "S" ''#{type uint32_t} [''Show, ''Storable, ''Num]
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
