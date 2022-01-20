{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Try.GetSizeAlignment where

import Vulkan.Pipeline.VertexInputState.GetSizeAlignment

sampleSizeAlignmentList1, sampleSizeAlignmentList2 :: Maybe [(Size, Alignment)]
sampleSizeAlignmentList1 = findSizeAlignmentList @(Int, Float, Bool, Char) @Bool
sampleSizeAlignmentList2 = findSizeAlignmentList @(Int, (), Float, Word, Bool, Char) @Bool

sampleFindOffset1, sampleFindOffset2 :: Maybe Offset
sampleFindOffset1 = findOffset @(Int, Float, Bool, Char) @Bool
sampleFindOffset2 = findOffset @(Int, (), Float, Word, Bool, Char) @Bool
