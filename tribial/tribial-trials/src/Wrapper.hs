{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Wrapper where

import Foreign.Ptr

-- foreign import ccall "wrapper" wrapAll :: a -> IO (FunPtr a)

foreign import ccall "wrapper" wrapInt :: Int -> IO (FunPtr Int)

foreign import ccall "wrapper" wrapIntInt :: (Int -> Int) -> IO (FunPtr (Int -> Int))

foreign import ccall "dynamic" makeInt :: FunPtr Int -> Int

foreign import ccall "dynamic" makeIntInt :: FunPtr (Int -> Int) -> Int -> Int
