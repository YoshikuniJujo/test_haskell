{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Buffer.View.Core where

import Foreign.Ptr

data VTag
type V = Ptr VTag

type PtrV = Ptr V
