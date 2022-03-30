{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.RenderPass where

import Vulkan.RenderPass.Enum

import qualified Vulkan.Attachment as Attachment
import qualified Vulkan.Subpass as Subpass

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoAttachments :: [Attachment.Description],
	createInfoSubpasses :: [Subpass.Description]
	-- TODO
	}
	deriving Show
