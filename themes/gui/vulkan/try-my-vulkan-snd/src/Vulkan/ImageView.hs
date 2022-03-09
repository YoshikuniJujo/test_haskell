{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.ImageView where

import Vulkan
import Vulkan.Enum
import Vulkan.ImageView.Enum

import qualified Vulkan.ImageView.Core as C

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoImage :: Image,
	createInfoViewType :: Type,
	createInfoFormat :: Format
	}
	deriving Show
