{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-missing-export-lists #-}

module Data.TypeLevel.Tuple.MapIndex where

import Data.TypeLevel.Tuple.MapIndex.TH

uncurry mkM `mapM` [ (i, n) | n <- [2 .. 10], i <- [0 .. n - 1] ]
