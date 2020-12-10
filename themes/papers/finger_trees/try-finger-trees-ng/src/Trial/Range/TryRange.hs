{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Range.TryRange where

import Data.List.Range

sample1 :: RangeL 2 5 Int
sample1 = 123 :. 456 :. 789 :.. NilL

sample2 :: RangeR 2 5 Int
sample2 = leftToRight sample1

sample3 :: RangeL 2 5 Int
sample3 = rightToLeft sample2
