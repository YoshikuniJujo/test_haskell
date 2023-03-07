{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Object where

import GHC.TypeNats
import Data.Kind.Object qualified as K

data Object = Static K.Object | Dynamic Nat K.Object
