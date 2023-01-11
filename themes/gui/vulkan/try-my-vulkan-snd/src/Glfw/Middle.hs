{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Glfw.Middle where

import Foreign.Marshal
import Foreign.Storable
import Foreign.Pointable
import Control.Monad.Cont

import Gpu.Vulkan.Exception.Middle.Internal

import qualified Graphics.UI.GLFW as GlfwB
import Gpu.Vulkan.AllocationCallbacks.Middle.Internal
	qualified as AllocationCallbacks
import qualified Gpu.Vulkan.Instance.Middle.Internal as Instance
import qualified Gpu.Vulkan.Khr.Surface.Middle.Internal as Surface

createWindowSurface :: Pointable n =>
	Instance.I -> GlfwB.Window -> Maybe (AllocationCallbacks.A n) -> IO Surface.S
createWindowSurface (Instance.I ist) win mac =
	($ pure) . runContT $ Surface.S <$> do
		pac <- AllocationCallbacks.maybeToCore mac
		psfc <- ContT alloca
		lift do	r <- GlfwB.createWindowSurface ist win pac psfc
			throwUnlessSuccess r
			peek psfc
