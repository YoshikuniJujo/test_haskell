{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Instance (
	I, create, M.CreateInfo(..),
	M.enumerateLayerProperties, M.enumerateExtensionProperties ) where

import Foreign.Storable.PeekPoke
import Control.Exception
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.Uncurry

import Gpu.Vulkan.Instance.Type

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.AllocationCallbacks.Type as AllocationCallbacks
import qualified Gpu.Vulkan.Instance.Middle as M

create :: (
	WithPoked (TMaybe.M n), WithPoked (TMaybe.M n2),
	AllocationCallbacks.ToMiddle' msn3n3 ) =>
	M.CreateInfo n n2 ->
	TPMaybe.M (U2 AllocationCallbacks.A) msn3n3 ->
	(forall s . I s -> IO a) -> IO a
create ci (AllocationCallbacks.toMiddle' -> mac) f =
	bracket (M.create ci mac) (`M.destroy` mac) (f . I)
