{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Khr.Surface where

import Foreign.Ptr
import Foreign.Storable
import Control.Monad.Cont

import Vulkan.Base
import Vulkan.Instance
import Vulkan.AllocationCallbacks

import qualified Vulkan.AllocationCallbacks.Internal as I

newtype Surface = Surface (Ptr Surface) deriving (Show, Storable)

destroySurface :: Pointable n =>
	Instance -> Surface -> Maybe (AllocationCallbacks n) -> IO ()
destroySurface ist sfc mac = ($ pure) $ runContT do
	piac <- case mac of
		Nothing -> pure NullPtr
		Just ac -> ContT $ withAllocationCallbacksPtr ac
	lift $ c_vkDestroySurfaceKHR ist sfc piac

foreign import ccall "vkDestroySurfaceKHR" c_vkDestroySurfaceKHR ::
	Instance -> Surface -> Ptr I.AllocationCallbacks -> IO ()
