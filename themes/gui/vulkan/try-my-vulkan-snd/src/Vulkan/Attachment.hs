{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Attachment where

import Vulkan.Enum
import Vulkan.Attachment.Enum

import qualified Vulkan.Sample.Enum as Sample
import qualified Vulkan.Image.Enum as Image
import qualified Vulkan.Attachment.Core as C

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
