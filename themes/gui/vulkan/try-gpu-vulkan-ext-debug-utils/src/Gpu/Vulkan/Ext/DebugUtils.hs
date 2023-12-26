{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Ext.DebugUtils (

	-- * EXTENSION NAME

	extensionName,

	-- * LABEL

	M.Label(..),

	-- * OBJECT NAME INFO

	M.ObjectNameInfo(..),
	M.ObjectNameInfoNoNext(..),
	M.ObjectNameInfoResult(..),

	-- * ENUM

	module Gpu.Vulkan.Ext.DebugUtils.Enum

	) where

import Gpu.Vulkan.Instance.Internal qualified as Ist
import Gpu.Vulkan.Ext.DebugUtils.Middle qualified as M
import Gpu.Vulkan.Ext.DebugUtils.Enum

extensionName :: Ist.ExtensionName
extensionName = Ist.ExtensionName M.extensionName
