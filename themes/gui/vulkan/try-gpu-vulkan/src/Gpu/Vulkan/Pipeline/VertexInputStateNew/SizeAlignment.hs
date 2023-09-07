{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module Gpu.Vulkan.Pipeline.VertexInputStateNew.SizeAlignment (
	wholeSizeAlignment, offsetOf,
	module Gpu.Vulkan.Pipeline.VertexInputStateNew.SizeAlignment.Internal, Offset ) where

import Gpu.Vulkan.Pipeline.VertexInputStateNew.SizeAlignment.Internal
import Gpu.Vulkan.Pipeline.VertexInputStateNew.SizeAlignment.TH

concat <$> instanceSizeAlignmentListTuple `mapM` filter (/= 1) [0 .. 15]
concat <$> instanceSizeAlignmentListUntilTuple `mapM` filter (/= 1) [0 .. 15]

wholeSizeAlignment :: forall a . SizeAlignmentList a => SizeAlignment
wholeSizeAlignment = let sas = sizeAlignmentList @a in
	(calcWholeSize sas, calcWholeAlignment sas)

calcWholeAlignment :: [SizeAlignment] -> Alignment
calcWholeAlignment = foldl lcm 1 . (snd <$>)

calcWholeSize :: [SizeAlignment] -> Size
calcWholeSize = foldl next 0 . rotateAlignmentL

type Offset = Int

offsetOf :: forall t ts . SizeAlignmentListUntil t ts => Maybe Offset
offsetOf = calcOffset <$> sizeAlignmentListUntil @t @ts

calcOffset :: [SizeAlignment] -> Offset
calcOffset = foldl next 0 . shiftAlignmentL

next :: Offset -> SizeAlignment -> Offset
next os (sz, algn) = ((os + sz - 1) `div` algn + 1) * algn

shiftAlignmentL :: [SizeAlignment] -> [SizeAlignment]
shiftAlignmentL [] = error "empty size and alignment list"
shiftAlignmentL sas = zip ss as where (ss, _ : as) = unzip sas

rotateAlignmentL :: [SizeAlignment] -> [SizeAlignment]
rotateAlignmentL [] = error "empty size and alignment list"
rotateAlignmentL sas = zip ss (as ++ [a]) where (ss, a : as) = unzip sas
