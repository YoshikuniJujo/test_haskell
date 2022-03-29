{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Attachment where

import Vulkan.Enum
import Vulkan.Attachment.Enum

import qualified Vulkan.Sample.Enum as Sample
import qualified Vulkan.Image.Enum as Image

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
