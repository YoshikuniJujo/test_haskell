{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Foldl where

import Control.Arrow
import Data.List

foldl2' :: (s -> a -> b -> s) -> s -> [a] -> [b] -> s
foldl2' f s as bs = foldl' (uncurry . f) s (zip as bs)

scanl2 :: (s -> a -> b -> s) -> s -> [a] -> [b] -> [s]
scanl2 f s as bs = scanl (uncurry . f) s (zip as bs)

type Size = Int
type Alignment = Int
type Offset = Int

nextOffset :: Offset -> Size -> Alignment -> Offset
nextOffset os sz algn = ((os + sz - 1) `div` algn + 1) * algn

offsets :: [(Size, Alignment)] -> [Offset]
offsets = uncurry (scanl2 nextOffset 0) . (tail `second`) . unzip
