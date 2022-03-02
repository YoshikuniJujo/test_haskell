{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Glfw where

import Foreign.Marshal
import Foreign.Storable
import Control.Monad.Cont

import Vulkan
import Vulkan.Base
import Vulkan.Exception
import Vulkan.AllocationCallbacks (AllocationCallbacks, maybeToCore)
import Vulkan.Khr

import qualified Graphics.UI.GLFW as GlfwB

createWindowSurface :: Pointable n =>
	Instance -> GlfwB.Window -> Maybe (AllocationCallbacks n) -> IO Surface
createWindowSurface (Instance ist) win mac =
	($ pure) . runContT $ Surface <$> do
		pac <- maybeToCore mac
		psfc <- ContT alloca
		lift do	r <- GlfwB.createWindowSurface ist win pac psfc
			throwUnlessSuccess r
			peek psfc
