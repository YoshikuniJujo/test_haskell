{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Ext.DebugUtils.Messenger (

	-- * CREATE

	create, M.CreateInfo(..), M.FnCallback, M.CallbackData(..)

	) where

import Foreign.Storable.PeekPoke
import Control.Exception
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.Tuple.Uncurry

import Gpu.Vulkan.PNext.Middle qualified as MI

import qualified Gpu.Vulkan.AllocationCallbacks.Internal as AllocationCallbacks
import qualified Gpu.Vulkan.Instance.Internal as Instance
import qualified Gpu.Vulkan.Ext.DebugUtils.Messenger.Middle as M

create :: (
	WithPoked (TMaybe.M mn),
	MI.FindChainAll cb, Storable' ud,
	AllocationCallbacks.ToMiddle mac ) =>
	Instance.I si -> M.CreateInfo mn cb ud ->
	TPMaybe.M (U2 AllocationCallbacks.A) mac -> IO a -> IO a
create (Instance.I ist) ci
	(AllocationCallbacks.toMiddle -> macc) f = bracket
	(M.create ist ci macc) (\m -> M.destroy ist m macc) (const f)
