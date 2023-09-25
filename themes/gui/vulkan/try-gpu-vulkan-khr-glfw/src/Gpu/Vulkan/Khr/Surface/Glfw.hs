{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr.Surface.Glfw (

	init, ErrorMessage, getRequiredInstanceExtensions,

	GlfwB.pollEvents, GlfwB.waitEvents,

	createWindowSurface

	) where

import Prelude hiding (init)

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.String
import Control.Exception
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.Tuple.Uncurry
import Data.Bool
import Data.Text qualified as Txt
import Data.Text.Foreign qualified as Txt

import qualified Graphics.UI.GLFW as GlfwB

import qualified Gpu.Vulkan.AllocationCallbacks.Internal as AllocationCallbacks
import qualified Gpu.Vulkan.Instance.Internal as Vk.Instance
import qualified Gpu.Vulkan.Khr.Surface.Type as Vk.Khr.Surface
import qualified Gpu.Vulkan.Khr.Surface.Middle as Vk.Khr.Surface.M
import qualified Gpu.Vulkan.Khr.Surface.Glfw.Middle as M

init :: (ErrorMessage -> IO a) -> IO a -> IO a
init hdl cmp = GlfwB.init >>= bool
	(hdl $ "Gpu.Vulkan.Khr.Surface.Glfw: " ++
		"GLFW-b.Graphics.UI.GLFW.init return False")
	(finally cmp GlfwB.terminate)

type ErrorMessage = String

createWindowSurface :: (AllocationCallbacks.ToMiddle mscc ) =>
	Vk.Instance.I si -> GlfwB.Window ->
	TPMaybe.M (U2 AllocationCallbacks.A) mscc ->
	(forall ss . Vk.Khr.Surface.S ss -> IO a) -> IO a
createWindowSurface (Vk.Instance.I ist) win
	(AllocationCallbacks.toMiddle -> macc) f = bracket
	(M.createWindowSurface ist win macc)
	(\sfc -> Vk.Khr.Surface.M.destroy ist sfc macc)
	(f . Vk.Khr.Surface.S)

getRequiredInstanceExtensions :: IO [Txt.Text]
getRequiredInstanceExtensions = (cstrToText `mapM`) =<< GlfwB.getRequiredInstanceExtensions

cstrToText :: CString -> IO Txt.Text
cstrToText cs = Txt.peekCStringLen =<< cstringToCStringLen cs

cstringLength :: CString -> IO Int
cstringLength pc = do
	c <- peek pc
	case c of
		0 -> pure 0
		_ -> (+ 1) <$> cstringLength (pc `plusPtr` 1)

cstringToCStringLen :: CString -> IO CStringLen
cstringToCStringLen cs = (cs ,) <$> cstringLength cs
