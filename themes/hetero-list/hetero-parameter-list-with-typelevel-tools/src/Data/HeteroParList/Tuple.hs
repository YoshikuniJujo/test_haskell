{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.HeteroParList.Tuple where

import Data.TypeLevel.Tuple.MapIndex qualified as TMapIndex
import Data.HeteroParList.Tuple.TH

concat <$> uncurry mkMap `mapM` [ (i, n) | n <- [2 .. 10], i <- [0 .. n - 1] ]
