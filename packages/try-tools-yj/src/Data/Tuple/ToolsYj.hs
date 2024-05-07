{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Tuple.ToolsYj (
	uncurryDup,
	mapTup3, appTup3, mapTup3M, appTup3M, mapTup3M_, appTup3M_
	) where

import Control.Monad

uncurryDup :: (a -> b -> c -> d) -> ((a, b), c) -> d
uncurryDup = uncurry . uncurry

mapTup3 :: (a -> b) -> (a, a, a) -> (b, b, b)
mapTup3 f = appTup3 f f f

appTup3 :: (a -> b) -> (c -> d) -> (e -> f) -> (a, c, e) -> (b, d, f)
appTup3 f g h (x, y, z) = (f x, g y, h z)

mapTup3M :: Applicative m => (a -> m b) -> (a, a, a) -> m (b, b, b)
mapTup3M f = appTup3M f f f

appTup3M :: Applicative m =>
	(a -> m b) -> (c -> m d) -> (e -> m f) -> (a, c, e) -> m (b, d, f)
appTup3M f g h (x, y, z) = (,,) <$> f x <*> g y <*> h z

mapTup3M_ :: Applicative m => (a -> m b) -> (a, a, a) -> m ()
mapTup3M_ f = void . mapTup3M f

appTup3M_ :: Applicative m =>
	(a -> m b) -> (c -> m d) -> (e -> m f) -> (a, c, e) -> m ()
appTup3M_ f g h = void . appTup3M f g h
