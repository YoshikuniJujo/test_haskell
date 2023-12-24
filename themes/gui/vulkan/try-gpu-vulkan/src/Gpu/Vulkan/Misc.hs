{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Misc (nil') where

import Data.Kind
import Data.TypeLevel.ParMaybe qualified as TPMaybe

nil' :: TPMaybe.M (t :: k -> Type) 'Nothing
nil' = TPMaybe.N
