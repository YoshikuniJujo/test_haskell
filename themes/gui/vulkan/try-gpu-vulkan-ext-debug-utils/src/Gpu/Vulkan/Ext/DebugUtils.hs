{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Ext.DebugUtils (

	-- * EXTENSION NAME

	extensionName,

	-- * LABEL

	Label(..),

	-- * OBJECT NAME INFO

	ObjectNameInfo(..), ObjectNameInfoNoNext(..), ObjectNameInfoResult(..)

	) where

import Gpu.Vulkan.Ext.DebugUtils.Middle
