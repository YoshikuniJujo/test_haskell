{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Pixels (colors, colorsA) where

import Foreign.C.Types
import Data.Tuple.ToolsYj
import Data.Maybe
import Data.List qualified as L
import Data.Array
import Data.Color
import System.Random

colors :: [[Rgb CDouble]]
colors = sep 5 randomColors

colorsA :: Array Int (Array Int (Rgb CDouble))
colorsA = listArray (0, 4) (listArray (0, 4) <$> colors)

sep :: Int -> [a] -> [[a]]
sep n = \case [] -> []; xs -> take n xs : sep n (drop n xs)

randomColors :: [Rgb CDouble]
randomColors = fromJust . uncurry3 rgbDouble . mapTup3 (/ 100) <$> L.unfoldr popTuple3 (randomRs (0, 100) (mkStdGen 8))

popTuple3 :: [a] -> Maybe ((a, a, a), [a])
popTuple3 (x : y : z : xs) = Just ((x, y, z), xs)
popTuple3 _ = Nothing
