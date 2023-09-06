{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Try.GetSizeAlignment where

import Foreign.Storable.SizeAlignment
-- import Vulkan.Pipeline.VertexInputState.GetSizeAlignment

sampleSizeAlignmentList1, sampleSizeAlignmentList2 :: Maybe [(Size, Alignment)]
sampleSizeAlignmentList1 = sizeAlignmentListUntil @Bool @(Int, Float, Bool, Char)
sampleSizeAlignmentList2 = sizeAlignmentListUntil @Bool @(Int, (), Float, Word, Bool, Char)

sampleFindOffset1, sampleFindOffset2 :: Maybe Offset
sampleFindOffset1 = offsetOf @Bool @(Int, Float, Bool, Char)
sampleFindOffset2 = offsetOf @Bool @(Int, (), Float, Word, Bool, Char)
