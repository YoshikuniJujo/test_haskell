{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module FloatClosure (printFloatClosure) where

import GHC.Exts.Heap (getClosureData)

printFloatClosure :: IO ()
printFloatClosure = print =<< getClosureData 123.4#
