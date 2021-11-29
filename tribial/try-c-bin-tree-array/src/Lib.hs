{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

foreign import ccall "foo" c_foo :: IO ()
