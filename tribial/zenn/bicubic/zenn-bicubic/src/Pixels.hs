{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Pixels (
	colors, colorsA, gray,

	nearestColors, linearColors, cubicColors,

	forXyv_, srcXs, srcYs, distXs, distYs
	) where

import GHC.Stack
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
randomColors = fromJust . uncurry3 rgbDouble . mapTup3 (/ 100)
	<$> L.unfoldr popTuple3 (randomRs (0, 100) (mkStdGen 8))

popTuple3 :: [a] -> Maybe ((a, a, a), [a])
popTuple3 (x : y : z : xs) = Just ((x, y, z), xs)
popTuple3 _ = Nothing

gray :: (Ord d, Fractional d) => Rgb d
gray = fromJust $ rgbDouble 0.5 0.5 0.5

nearestColors, linearColors, cubicColors :: [[Rgb CDouble]]
nearestColors = interpolate nearest distXs distYs colorsA
linearColors = interpolate linear distXs distYs colorsA
cubicColors = interpolate cubic distXs distYs colorsA

nearest, linear, cubic :: CDouble -> CDouble
nearest x = if (x < 0.5) then 1 else 0

linear x = if (x < 1) then 1 - x else 0

cubic x	| x < 1 = (3 * x ^ (3 :: Int) - 5 * x ^ (2 :: Int) + 2) / 2
	| otherwise = (- x ^ (3 :: Int) + 5 * x ^ (2 :: Int) - 8 * x + 4) / 2

forXyv_ :: Applicative m => [d] -> [d] -> [[v]] -> (d -> d -> v -> m a) -> m ()
forXyv_ xs ys vss f =
	for_ (zip ys vss) \(y, vs) -> for_ (zip xs vs) \(x, v) -> f x y v

srcXs, srcYs :: [CDouble]
srcXs = [0 .. 4]; srcYs = [0 .. 4]

distXs, distYs :: [CDouble]
distXs = [1, 1 + 1 / 3 .. 3]; distYs = [1, 1 + 1 / 3 .. 3]

interpolate :: (CDouble -> CDouble) -> [CDouble] -> [CDouble] ->
	Array Int (Array Int (Rgb CDouble)) -> [[Rgb CDouble]]
interpolate f xs ys caa = (<$> ys) \y -> (<$> xs) \x ->
	let	(xi, xd) = properFraction x
		(yi, yd) = properFraction y
		css = (<$> [yi - 1 .. yi + 2]) \a ->
			(<$> [xi - 1 .. xi + 2]) \b -> caa ! a ! b
		css' = (<$> zip [-1 .. 2] css) \(yi', cs) ->
			(<$> zip [-1 .. 2] cs) \(xi', c) ->
				c `mul` (f (abs $ yd - yi') * f (abs $ xd - xi')) in
		uncurry3 rgbDouble' . foldl add (0, 0, 0) $ concat css'

mul :: HasCallStack => Rgb CDouble -> CDouble -> (CDouble, CDouble, CDouble)
mul (RgbDouble r g b) n = (r * n, g * n, b * n)

type RgbRaw = (CDouble, CDouble, CDouble)

add :: RgbRaw -> RgbRaw -> RgbRaw
add (r1, g1, b1)  (r2, g2, b2) = (r1 +! r2, g1 +! g2, b1 +! b2)

(+!) :: CDouble -> CDouble -> CDouble
a +! b	| a + b < 0 = 0
	| a + b < 1 = a + b
	| otherwise = a + b

rgbDouble' :: (HasCallStack, Ord d, Num d) => d -> d -> d -> Rgb d
rgbDouble' r g b = fromJust $ rgbDouble r' g' b'
	where
	to01 x	| x < 0 = 0
		| x > 1 = 1
		| otherwise = x
	r' = to01 r; g' = to01 g; b' = to01 b
