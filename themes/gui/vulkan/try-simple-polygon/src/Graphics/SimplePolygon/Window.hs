{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.SimplePolygon.Window (
	create, W(..), windowSize, windowName ) where

import Data.IORef

import Graphics.UI.GLFW qualified as Glfw

data W = W Glfw.Window FramebufferResized

create :: (Int, Int) -> String -> (W -> IO a) -> IO a
create sz nm f = do
	fbrszd <- newFramebufferResized
	(\f' -> withWindow sz nm f' fbrszd) \w -> f $ W w fbrszd

withWindow :: (Int, Int) -> String -> (Glfw.Window -> IO a) -> FramebufferResized -> IO a
withWindow sz nm f g = initWindow sz nm g >>= \w ->
	f w <* (Glfw.destroyWindow w >> Glfw.terminate)

initWindow :: (Int, Int) -> String -> FramebufferResized -> IO Glfw.Window
initWindow sz nm frszd = do
	Just w <- do
		True <- Glfw.init
		Glfw.windowHint $ Glfw.WindowHint'ClientAPI Glfw.ClientAPI'NoAPI
		uncurry Glfw.createWindow sz nm Nothing Nothing
	w <$ Glfw.setFramebufferSizeCallback
		w (Just $ \_ _ _ -> writeIORef frszd True)

type FramebufferResized = IORef Bool

newFramebufferResized :: IO FramebufferResized
newFramebufferResized = newIORef False

windowSize :: (Int, Int)
windowSize = (width, height) where width = 800; height = 600

windowName :: String
windowName = "Triangle"
