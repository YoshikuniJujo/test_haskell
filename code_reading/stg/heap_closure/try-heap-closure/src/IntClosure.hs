{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module IntClosure (printIntClosure) where

import GHC.Exts.Heap (getClosureData)

printIntClosure :: IO ()
printIntClosure = print =<< getClosureData 123#
