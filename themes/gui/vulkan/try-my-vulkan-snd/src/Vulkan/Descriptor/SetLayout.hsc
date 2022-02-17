{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Descriptor.SetLayout where

import Foreign.Ptr

data SetLayoutTag
type SetLayout = Ptr SetLayoutTag

type PtrSetLayout = Ptr SetLayout
