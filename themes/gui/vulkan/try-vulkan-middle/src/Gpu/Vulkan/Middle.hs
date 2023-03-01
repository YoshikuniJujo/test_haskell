{-# LANGUAGE PatternSynonyms #-}
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

	-- * Rect, Offset and Extent

	Rect2d, pattern Rect2d, rect2dExtent, rect2dOffset,

	Offset3d, pattern Offset3d, offset3dX, offset3dY, offset3dZ,

	Extent2d, pattern Extent2d, extent2dWidth, extent2dHeight,
	Extent3d, pattern Extent3d, extent3dWidth, extent3dHeight, extent3dDepth,

	-- * Object Handle

	ObjectHandle,

	-- * Others

	FormatProperties(..),

	) where

import Gpu.Vulkan.Middle.Internal
