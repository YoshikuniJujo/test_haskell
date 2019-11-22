{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ShowLazyList where

import GHC.Exts.Heap (ClosureType(..), getClosureData, tipe, info)

getTipe :: a -> IO ClosureType
getTipe = (tipe . info <$>) . getClosureData
