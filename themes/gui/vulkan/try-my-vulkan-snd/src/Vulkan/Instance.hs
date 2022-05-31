{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Instance (
	I, create, M.CreateInfo(..), M.createInfoNil ) where

import Foreign.Pointable
import Control.Exception

import qualified Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Vulkan.Instance.Middle as M

newtype I s = I { unI :: M.I } deriving Show

create :: (Pointable n, Pointable n2, Pointable n3, Pointable n4) =>
	M.CreateInfo n n2 ->
	Maybe (AllocationCallbacks.A n3) -> Maybe (AllocationCallbacks.A n4) ->
	(forall s . I s -> IO a) -> IO a
create ci macc macd f = bracket (M.create ci macc) (`M.destroy` macd) (f . I)
