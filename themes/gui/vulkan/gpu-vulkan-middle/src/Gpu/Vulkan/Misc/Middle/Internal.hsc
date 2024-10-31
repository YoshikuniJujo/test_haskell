{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Misc.Middle.Internal (

	-- * Control

	mapContM,

	) where

#include <vulkan/vulkan.h>

mapContM :: Monad m => (a -> (b -> m c) -> m c) -> [a] -> ([b] -> m c) -> m c
-- mapContM f = runContT . mapM (ContT . f)
mapContM _ [] g = g []
mapContM f (x : xs) g = f x \y -> mapContM f xs \ys -> g $ y : ys
