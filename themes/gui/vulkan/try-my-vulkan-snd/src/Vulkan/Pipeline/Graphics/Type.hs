{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.Graphics.Type where

import qualified Vulkan.Pipeline.Graphics.Middle as M

newtype GList s vs's tss = GList (M.PList vs's tss) deriving Show

pattern GNil :: GList s vs's tss
pattern GNil <- GList M.PNil

pattern GCons ::
	M.G vs ts -> GList s vs's tss -> GList s (vs ': vs's) (ts ': tss)
pattern g `GCons` gs <- GList (g `M.PCons` (GList -> gs))
