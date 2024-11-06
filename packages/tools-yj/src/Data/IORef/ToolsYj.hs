{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.IORef.ToolsYj (newDefaultIORef, checkFlag) where

import Data.Default
import Data.Bool
import Data.IORef

newDefaultIORef :: Default a => IO (IORef a)
newDefaultIORef = newIORef def

checkFlag :: IORef Bool -> IO Bool
checkFlag flg =
	readIORef flg >>= bool (pure False) (True <$ writeIORef flg False)
