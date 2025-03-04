{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Foreign.Ptr
import Control.Monad
import System.IO

import Graphics.UI.GlfwG qualified as GlfwG
import Graphics.UI.GlfwG.Window qualified as GlfwG.Win
import Graphics.UI.GlfwG.Window.Type qualified as GlfwG.Win

import Graphics.UI.GLFW.C qualified as GlfwC

import Bindings.GLFW qualified as GlfwBase

main :: IO ()
main = GlfwG.setErrorCallback (Just glfwErrorCallback) >>
	GlfwG.init error do
	GlfwG.Win.hint
		$ GlfwG.Win.WindowHint'ClientAPI GlfwG.Win.ClientAPI'NoAPI
	GlfwG.Win.create 1280 720
		"Dear ImGui GLFW+Vulkan example" Nothing Nothing \win -> do
		vs <- GlfwG.vulkanSupported
		when (not vs) $ error "GLFW: Vulkan Not Supported"
		mainCxx win

glfwErrorCallback :: GlfwG.Error -> GlfwG.ErrorMessage -> IO ()
glfwErrorCallback err dsc =
	hPutStrLn stderr $ "GLFW Error " ++ show err ++ ": " ++ dsc

foreign import ccall "main_cxx" cxx_main_cxx :: Ptr GlfwBase.C'GLFWwindow  -> IO ()

mainCxx :: GlfwG.Win.W sw -> IO ()
mainCxx (GlfwG.Win.W win) = cxx_main_cxx (GlfwC.toC win)
