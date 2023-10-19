{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.UI.GlfwG.Window.Type where

import Graphics.UI.GLFW qualified as B

newtype W s = W B.Window deriving (Eq, Ord, Show)
