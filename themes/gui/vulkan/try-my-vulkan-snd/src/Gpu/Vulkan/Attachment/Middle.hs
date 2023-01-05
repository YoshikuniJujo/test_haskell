{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Attachment.Middle (
	Description(..), DescriptionFlags, DescriptionFlagBits,

	LoadOp,
	pattern LoadOpLoad, pattern LoadOpClear, pattern LoadOpDontCare,
	pattern LoadOpNoneExt, pattern LoadOpMaxEnum,

	StoreOp,
	pattern StoreOpStore, pattern StoreOpDontCare, pattern StoreOpNone,
	pattern StoreOpNoneKhr, pattern StoreOpNoneQcom, pattern StoreOpNoneExt,
	pattern StoreOpMaxEnum,

	Reference(..), A ) where

import Gpu.Vulkan.Attachment.Enum
import Gpu.Vulkan.Attachment.Middle.Internal
