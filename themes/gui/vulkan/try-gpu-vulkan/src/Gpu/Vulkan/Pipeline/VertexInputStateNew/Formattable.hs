{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.VertexInputStateNew.Formattable where

import Gpu.Vulkan.Enum

class Formattable a where formatOf :: Format
instance Formattable Int where formatOf = FormatUndefined
