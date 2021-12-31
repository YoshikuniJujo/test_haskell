{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Glfw where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Control.Monad.Cont

import qualified Graphics.UI.GLFW as GlfwB

import Vulkan.Base
import Vulkan.Exception
import qualified Vulkan.Instance as Vk
import qualified Vulkan.AllocationCallbacks as Vk

newtype Surface = Surface (Ptr Surface) deriving (Show, Storable)

createWindowSurface :: Pointable n =>
	Vk.Instance -> GlfwB.Window -> Maybe (Vk.AllocationCallbacks n) -> IO Surface
createWindowSurface (Vk.Instance pist) w mac = ($ pure) $ runContT do
	piac <- case mac of
		Nothing -> pure NullPtr
		Just ac -> ContT $ Vk.withAllocationCallbacksPtr ac
	ps <- ContT alloca
	lift do	r <- GlfwB.createWindowSurface pist w piac ps
		throwUnlessSuccess r
		peek ps
