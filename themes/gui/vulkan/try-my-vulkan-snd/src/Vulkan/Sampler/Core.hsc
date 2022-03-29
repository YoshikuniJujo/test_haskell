{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Sampler.Core where

import Foreign.Ptr

data STag
type S = Ptr STag

type PtrS = Ptr S
