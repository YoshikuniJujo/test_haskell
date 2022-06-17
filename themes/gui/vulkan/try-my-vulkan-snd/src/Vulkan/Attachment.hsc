{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Attachment where

import Foreign.Storable
import Foreign.C.Enum
import Data.Word

import Vulkan.Enum
import Vulkan.Attachment.Enum

import qualified Vulkan.Sample.Enum as Sample
import qualified Vulkan.Image.Enum as Image
import qualified Vulkan.Attachment.Core as C

#include <vulkan/vulkan.h>

data Description = Description {
	descriptionFlags :: DescriptionFlags,
	descriptionFormat :: Format,
	descriptionSamples :: Sample.CountFlagBits,
	descriptionLoadOp :: LoadOp,
	descriptionStoreOp :: StoreOp,
	descriptionStencilLoadOp :: LoadOp,
	descriptionStencilStoreOp :: StoreOp,
	descriptionInitialLayout :: Image.Layout,
	descriptionFinalLayout :: Image.Layout }
	deriving Show

descriptionToCore :: Description -> C.Description
descriptionToCore Description {
	descriptionFlags = DescriptionFlagBits flgs,
	descriptionFormat = Format fmt,
	descriptionSamples = Sample.CountFlagBits smps,
	descriptionLoadOp = LoadOp lo,
	descriptionStoreOp = StoreOp so,
	descriptionStencilLoadOp = LoadOp slo,
	descriptionStencilStoreOp = StoreOp sso,
	descriptionInitialLayout = Image.Layout il,
	descriptionFinalLayout = Image.Layout fl
	} = C.Description {
		C.descriptionFlags = flgs,
		C.descriptionFormat = fmt,
		C.descriptionSamples = smps,
		C.descriptionLoadOp = lo,
		C.descriptionStoreOp = so,
		C.descriptionStencilLoadOp = slo,
		C.descriptionStencilStoreOp = sso,
		C.descriptionInitialLayout = il,
		C.descriptionFinalLayout = fl }

enum "A" ''#{type uint32_t} [''Show, ''Storable, ''Num]
	[("AUnused", #{const VK_ATTACHMENT_UNUSED})]

data Reference = Reference {
	referenceAttachment :: A,
	referenceLayout :: Image.Layout }
	deriving Show

referenceToCore :: Reference -> C.Reference
referenceToCore Reference {
	referenceAttachment = A a,
	referenceLayout = Image.Layout lyt } = C.Reference {
		C.referenceAttachment = a,
		C.referenceLayout = lyt }
