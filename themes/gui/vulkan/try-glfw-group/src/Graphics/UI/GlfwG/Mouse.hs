{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.UI.GlfwG.Mouse (

	getButton, B.MouseButton(..), B.MouseButtonState(..),
	getCursorPos

	) where

import Graphics.UI.GLFW qualified as B
import Graphics.UI.GlfwG.Window.Type qualified as Win

getButton :: Win.W sw -> B.MouseButton -> IO B.MouseButtonState
getButton (Win.W w) = B.getMouseButton w

getCursorPos :: Win.W sw -> IO (Double, Double)
getCursorPos (Win.W w) = B.getCursorPos w
