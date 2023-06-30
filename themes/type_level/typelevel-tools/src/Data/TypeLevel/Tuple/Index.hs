{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.TypeLevel.Tuple.Index where

import Data.TypeLevel.Tuple.Index.TH

uncurry mkI `mapM` [ (i, n) | n <- [2 .. 10], i <- [0 .. n - 1] ]
