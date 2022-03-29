{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.DescriptorSet.Layout.Core where

import Foreign.Ptr

data LTag
type L = Ptr LTag

type PtrL = Ptr L
