{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.NoVulkan.Io.Middle (

	C.get, C.I(..),

	getConfigFlags, setConfigFlags, modifyConfigFlags

	) where

import Gpu.Vulkan.ImGui.NoVulkan.Enum qualified as E
import Gpu.Vulkan.ImGui.NoVulkan.Io.Core qualified as C

getConfigFlags :: C.I -> IO E.ConfigFlags
getConfigFlags i = E.ConfigFlags <$> C.getConfigFlags i

setConfigFlags :: C.I -> E.ConfigFlags -> IO ()
setConfigFlags i (E.ConfigFlags fs) = C.setConfigFlags i fs

modifyConfigFlags :: C.I -> (E.ConfigFlags -> E.ConfigFlags) -> IO ()
modifyConfigFlags i f = setConfigFlags i . f =<< getConfigFlags i
