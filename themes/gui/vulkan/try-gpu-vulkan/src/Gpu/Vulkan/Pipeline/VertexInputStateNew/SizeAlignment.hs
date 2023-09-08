{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module Gpu.Vulkan.Pipeline.VertexInputStateNew.SizeAlignment (
	wholeSizeAlignment,
	module Gpu.Vulkan.Pipeline.VertexInputStateNew.SizeAlignment.Internal, Offset ) where

import Gpu.Vulkan.Pipeline.VertexInputStateNew.SizeAlignment.Internal

wholeSizeAlignment :: forall a . SizeAlignmentList a => SizeAlignment
wholeSizeAlignment = let sas = sizeAlignmentList @a in
	(calcWholeSize sas, calcWholeAlignment sas)

calcWholeAlignment :: [SizeAlignment] -> Alignment
calcWholeAlignment = foldl lcm 1 . (snd <$>)

calcWholeSize :: [SizeAlignment] -> Size
calcWholeSize = foldl next 0 . rotateAlignmentL

type Offset = Int

next :: Offset -> SizeAlignment -> Offset
next os (sz, algn) = ((os + sz - 1) `div` algn + 1) * algn

rotateAlignmentL :: [SizeAlignment] -> [SizeAlignment]
rotateAlignmentL [] = error "empty size and alignment list"
rotateAlignmentL sas = zip ss (as ++ [a]) where (ss, a : as) = unzip sas
