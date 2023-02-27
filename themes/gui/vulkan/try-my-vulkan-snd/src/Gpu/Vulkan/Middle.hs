{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Middle (

	-- * Application Info and Api Version

	ApplicationInfo(..), ApiVersion, makeApiVersion, apiVersion_1_0,

	-- * Properties

	LayerProperties(..), ExtensionProperties(..),

	-- * Submit Info

	SubmitInfo(..), SubmitInfoListToCore(..),

	-- * Stencil Op State and Clear

	-- ** Stencil Op State

	StencilOpState(..),

	-- ** Clear Value

	ClearValue(..), ClearValueListToCore,
	ClearValueToCore, ClearColorValueToCore,

	-- ** Clear Type

	ClearType(..),
	ClearColorType(..),

	-- * Object Handle

	ObjectHandle ) where

import Gpu.Vulkan.Middle.Internal
