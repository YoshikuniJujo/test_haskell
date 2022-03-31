{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Glfw where

import Foreign.Marshal
import Foreign.Storable
import Foreign.Pointable
import Control.Monad.Cont

import Vulkan
import Vulkan.Exception
import Vulkan.Khr

import qualified Graphics.UI.GLFW as GlfwB
import qualified Vulkan.AllocationCallbacks as AllocationCallbacks

createWindowSurface :: Pointable n =>
	Instance -> GlfwB.Window -> Maybe (AllocationCallbacks.A n) -> IO Surface
createWindowSurface (Instance ist) win mac =
	($ pure) . runContT $ Surface <$> do
		pac <- AllocationCallbacks.maybeToCore mac
		psfc <- ContT alloca
		lift do	r <- GlfwB.createWindowSurface ist win pac psfc
			throwUnlessSuccess r
			peek psfc
