{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.QueryPool.Type (

	-- * QUERY POOL

	Q(..)

	) where

import Data.Kind

import Gpu.Vulkan.QueryPool.Middle qualified as M

newtype Q sq (tp :: Bool -> Type) = Q M.Q deriving Show
