{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Tuple.ToolsYj (
	uncurryDup,
	appTup3, mapTup3
	) where

uncurryDup :: (a -> b -> c -> d) -> ((a, b), c) -> d
uncurryDup = uncurry . uncurry

appTup3 :: (a -> b) -> (c -> d) -> (e -> f) -> (a, c, e) -> (b, d, f)
appTup3 f g h (x, y, z) = (f x, g y, h z)

mapTup3 :: (a -> b) -> (a, a, a) -> (b, b, b)
mapTup3 f = appTup3 f f f
