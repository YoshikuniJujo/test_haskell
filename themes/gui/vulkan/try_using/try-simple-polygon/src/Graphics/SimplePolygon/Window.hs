{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.SimplePolygon.Window (create, W(..)) where

import Data.Tuple.ToolsYj
import Data.Function.ToolsYj
import Data.IORef

import Graphics.UI.GlfwG qualified as GlfwG
import Graphics.UI.GlfwG.Window qualified as GlfwG.Win

create :: (Int, Int) -> String -> (forall sw . W sw -> IO a) -> IO a
create sz nm f = newIORef False >>= \fr -> withWin sz nm fr \w -> f $ W w fr

data W sw = W (GlfwG.Win.W sw) FramebufferResized
type FramebufferResized = IORef Bool

withWin :: (Int, Int) -> String ->
	FramebufferResized -> (forall s . GlfwG.Win.W s -> IO a) -> IO a
withWin sz nm fr a = GlfwG.init error
	$ GlfwG.Win.group \g -> a =<< initWin (sz, nm) fr g

initWin :: ((Int, Int), String) ->
	FramebufferResized -> GlfwG.Win.Group s () -> IO (GlfwG.Win.W s)
initWin sizeName fr g = do
	Right w <- do
		GlfwG.Win.hint noApi
		uncurryDup (GlfwG.Win.create' g ()) sizeName Nothing Nothing
	w <$ GlfwG.Win.setFramebufferSizeCallback
		w (Just . const3 $ writeIORef fr True)
	where noApi = GlfwG.Win.WindowHint'ClientAPI GlfwG.Win.ClientAPI'NoAPI
