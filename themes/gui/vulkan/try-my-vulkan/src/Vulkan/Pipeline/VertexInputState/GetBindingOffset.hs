{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.VertexInputState.GetBindingOffset where

import Foreign.Storable.SizeAlignment
import Data.Kind

class BindingOffsetList (ts :: [Type]) t where
	bindingOffsetList :: Maybe (Int, Offset)
