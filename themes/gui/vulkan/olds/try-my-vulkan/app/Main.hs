{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.Fix
import Data.Bool
import Data.List.Length

import qualified Graphics.UI.GLFW as Glfw

import qualified Vulkan.Instance as Vk
import qualified Cglm as Glm

main :: IO ()
main = do
	True <- Glfw.init
	Glfw.windowHint $ Glfw.WindowHint'ClientAPI Glfw.ClientAPI'NoAPI
	Just window <- Glfw.createWindow 800 600 "Vulkan window" Nothing Nothing

	extensions <- Vk.enumerateInstanceExtensionProperties Nothing
	print $ length extensions

	let	matrix = Glm.Mat4 $
			(Glm.Vec4 $ 1 :. 2 :. 3 :. 4 :. NilL) :.
			(Glm.Vec4 $ 5 :. 6 :. 7 :. 8 :. NilL) :.
			(Glm.Vec4 $ 9 :. 10 :. 11 :. 12 :. NilL) :.
			(Glm.Vec4 $ 13 :. 14 :. 15 :. 16 :. NilL) :. NilL
		vec = Glm.Vec4 $ 1 :. 2 :. 3 :. 4 :. NilL
	print $ Glm.mat4Mulv matrix vec

	fix \loop -> bool (pure ()) loop =<< do
		Glfw.pollEvents
		not <$> Glfw.windowShouldClose window

	Glfw.destroyWindow window
	Glfw.terminate
