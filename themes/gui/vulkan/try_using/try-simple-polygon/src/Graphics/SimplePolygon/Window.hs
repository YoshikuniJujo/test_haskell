{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.SimplePolygon.Window (
	create, W(..), windowSize, windowName ) where

import Data.IORef

import Data.Tuple.ToolsYj
import Data.Function.ToolsYj

import Graphics.UI.GlfwG qualified as GlfwG
import Graphics.UI.GlfwG.Window qualified as GlfwG.Win

data W sw = W (GlfwG.Win.W sw) FramebufferResized

create :: (Int, Int) -> String -> (forall sw . W sw -> IO a) -> IO a
create sz nm f = do
	fbrszd <- newFramebufferResized
	withWindow sz nm fbrszd \w -> f $ W w fbrszd

withWindow :: (Int, Int) -> String -> FramebufferResized -> (forall s . GlfwG.Win.W s -> IO a) -> IO a
withWindow sz nm fr a = GlfwG.init error $ GlfwG.Win.group \g -> a =<< initWindow (sz, nm) fr g

initWindow :: ((Int, Int), String) -> FramebufferResized -> GlfwG.Win.Group s () -> IO (GlfwG.Win.W s)
initWindow sizeName fr g = do
	Right w <- do
		GlfwG.Win.hint noApi
		uncurryDup (GlfwG.Win.create' g ()) sizeName Nothing Nothing
	w <$ GlfwG.Win.setFramebufferSizeCallback
		w (Just . const3 $ writeIORef fr True)
	where
	noApi = GlfwG.Win.WindowHint'ClientAPI GlfwG.Win.ClientAPI'NoAPI

type FramebufferResized = IORef Bool

newFramebufferResized :: IO FramebufferResized
newFramebufferResized = newIORef False

windowSize :: (Int, Int)
windowSize = (width, height) where width = 800; height = 600

windowName :: String
windowName = "Triangle"
