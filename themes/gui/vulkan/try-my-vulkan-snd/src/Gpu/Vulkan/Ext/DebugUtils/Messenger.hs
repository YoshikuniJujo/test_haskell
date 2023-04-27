{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Ext.DebugUtils.Messenger (
	create, M, M.CreateInfo(..), M.FnCallback, M.CallbackData(..) ) where

import Foreign.Storable
import Foreign.Storable.PeekPoke
import Control.Exception
import Data.TypeLevel.Maybe qualified as TMaybe

import Gpu.Vulkan.Ext.DebugUtils.Messenger.Type

import Gpu.Vulkan.Middle.Internal qualified as MI

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Instance.Type as Instance
import qualified Gpu.Vulkan.Ext.DebugUtils.Messenger.Middle as M

create :: (
	WithPoked (TMaybe.M mn), MI.FindPNextChainAll n2, Storable n3, Storable n4, Storable n5,
	Storable ud, Pokable ud, Peek ud, Pokable c, Pokable d ) =>
	Instance.I si -> M.CreateInfo mn n2 n3 n4 n5 ud ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	(forall s . M s -> IO a) -> IO a
create (Instance.I ist) ci macc macd f = bracket
	(M.create ist ci macc) (\m -> M.destroy ist m macd) (f . M)
