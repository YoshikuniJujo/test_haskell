{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Instance.Internal (

	-- * CREATE

	create, I(..), M.CreateInfo(..),

	-- * ENUMERATE

	M.enumerateLayerProperties, M.enumerateExtensionProperties

	) where

import Foreign.Storable.PeekPoke
import Control.Exception
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.Tuple.Uncurry

import Gpu.Vulkan.Instance.Type

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.AllocationCallbacks.Type as AllocationCallbacks
import qualified Gpu.Vulkan.Instance.Middle as M

create :: (
	WithPoked (TMaybe.M mn), WithPoked (TMaybe.M ai),
	AllocationCallbacks.ToMiddle mac ) =>
	M.CreateInfo mn ai ->
	TPMaybe.M (U2 AllocationCallbacks.A) mac ->
	(forall s . I s -> IO a) -> IO a
create ci (AllocationCallbacks.toMiddle -> mac) f =
	bracket (M.create ci mac) (`M.destroy` mac) (f . I)
