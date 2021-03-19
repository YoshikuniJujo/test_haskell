{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal

main :: IO ()
main = alloca \p -> do
	print @(Ptr Int) p
	fp <- newForeignPtr p . putStrLn $ "finalize: " ++ show p
	withForeignPtr fp \p' -> print p'
