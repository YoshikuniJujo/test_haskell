{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Fence.Internal (

	-- * CREATE

	create, F(..), M.CreateInfo(..),

	-- * WAIT FOR FENCES AND RESET FENCES

	waitForFs, resetFs

	) where

import Foreign.Storable.PeekPoke
import Control.Exception
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.Tuple.Uncurry
import Data.HeteroParList qualified as HeteroParList
import Data.Word
import Data.Time

import Gpu.Vulkan.Fence.Type

import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.AllocationCallbacks.Type as AllocationCallbacks
import qualified Gpu.Vulkan.Fence.Middle as M

create :: (WithPoked (TMaybe.M mn), AllocationCallbacks.ToMiddle mac) =>
	Device.D sd -> M.CreateInfo mn ->
	TPMaybe.M (U2 AllocationCallbacks.A) mac ->
	(forall s . F s -> IO a) -> IO a
create (Device.D dvc) ci (AllocationCallbacks.toMiddle -> mac) f =
	bracket (M.create dvc ci mac) (\fnc -> M.destroy dvc fnc mac) (f . F)

waitForFs :: Device.D sd -> HeteroParList.PL F sfs -> Bool -> Maybe DiffTime -> IO ()
waitForFs (Device.D dvc) fs wa (maybe maxBound diffTimeToNanoseconds -> to) =
	M.waitForFs dvc (HeteroParList.toList (\(F f) -> f) fs) wa to

resetFs :: Device.D sd -> HeteroParList.PL F sfs -> IO ()
resetFs (Device.D dvc) = M.resetFs dvc . HeteroParList.toList \(F f) -> f

diffTimeToNanoseconds :: DiffTime -> Word64
diffTimeToNanoseconds = fromInteger . (`div` 1000) . diffTimeToPicoseconds