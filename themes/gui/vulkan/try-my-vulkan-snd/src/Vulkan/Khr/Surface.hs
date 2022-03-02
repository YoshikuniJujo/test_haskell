{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Khr.Surface where

import Control.Monad.Cont

import Vulkan
import Vulkan.Base
import Vulkan.Khr
import Vulkan.AllocationCallbacks (AllocationCallbacks, maybeToCore)

import qualified Vulkan.Khr.Surface.Core as C

destroy :: Pointable n =>
	Instance -> Surface -> Maybe (AllocationCallbacks n) -> IO ()
destroy (Instance ist) (Surface sfc) mac = ($ pure) $ runContT do
	pac <- maybeToCore mac
	lift $ C.destroy ist sfc pac
