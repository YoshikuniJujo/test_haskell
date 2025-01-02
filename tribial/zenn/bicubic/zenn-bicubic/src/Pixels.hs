{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Pixels (
	colors, colorsA, gray,

	nearestColors,

	forXyv_, srcXs, srcYs, distXs, distYs
	) where

import Foreign.C.Types
import Data.Foldable
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

gray :: (Ord d, Fractional d) => Rgb d
gray = fromJust $ rgbDouble 0.5 0.5 0.5

nearestColors :: [[Rgb CDouble]]
nearestColors = (<$> [1 :: Double, 1 + 1 / 3 .. 3]) \y ->
	(<$> [1 :: Double, 1 + 1 / 3 .. 3]) \x -> colorsA ! round y ! round x

forXyv_ :: Applicative m => [d] -> [d] -> [[v]] -> (d -> d -> v -> m a) -> m ()
forXyv_ xs ys vss f =
	for_ (zip ys vss) \(y, vs) -> for_ (zip xs vs) \(x, v) -> f x y v

srcXs, srcYs :: [CDouble]
srcXs = [0 .. 4]; srcYs = [0 .. 4]

distXs, distYs :: [CDouble]
distXs = [1, 1 + 1 / 3 .. 3]; distYs = [1, 1 + 1 / 3 .. 3]
