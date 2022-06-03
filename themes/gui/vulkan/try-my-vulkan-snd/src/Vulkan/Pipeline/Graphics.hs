{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.Graphics (
	createGs, M.CreateInfoList(..), M.CreateInfo(..),
	GList, pattern GNil, pattern GCons ) where

import Foreign.Pointable
import Control.Exception

import Vulkan.Pipeline.Graphics.Type

import qualified Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Vulkan.Device.Type as Device
import qualified Vulkan.Pipeline.Graphics.Middle as M
import qualified Vulkan.Pipeline.Cache.Type as Cache

createGs :: (
	M.CreateInfoListToCore
		ns n1s skndss vsss n2s vs's tss n3s n4s n5s n6s n7s n8s n9s n10s
		vs''s ts's,
	M.PListFromCore vs's tss, Pointable n', Pointable n'' ) =>
	Device.D sd -> Maybe (Cache.C sc) ->
	M.CreateInfoList
		ns n1s skndss vsss n2s vs's tss n3s n4s n5s n6s n7s n8s n9s n10s
		vs''s ts's ->
	Maybe (AllocationCallbacks.A n') -> Maybe (AllocationCallbacks.A n'') ->
	(forall s . GList s vs's tss -> IO a) -> IO a
createGs (Device.D dvc) ((Cache.cToMiddle <$>) -> mc) ci macc macd f = bracket
	(M.create dvc mc ci macc) (\gs -> M.destroyGs dvc gs macd) (f . GList)
