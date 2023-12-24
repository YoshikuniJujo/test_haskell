{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.IORef.ToolsYj (newDefaultIORef) where

import Data.Default
import Data.IORef

newDefaultIORef :: Default a => IO (IORef a)
newDefaultIORef = newIORef def
