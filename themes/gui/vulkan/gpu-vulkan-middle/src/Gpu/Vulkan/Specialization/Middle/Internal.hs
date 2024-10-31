{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Specialization.Middle.Internal (infoToCore) where

import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable.HeteroList hiding (alignments)
import qualified Data.HeteroParList as HeteroParList

import qualified Gpu.Vulkan.Specialization.Core as C

type Parameters = (Int, [(Int, Int, Int)], Int, Int)

parameters :: [(Int, Int)] -> Parameters
parameters szals = (
	length szals, zip3 [0 ..] ofs (fst <$> szals), tsz, alignments szals )
	where
	ofs = offsets szals; szs = fst <$> szals; tsz = last ofs + last szs

offsets :: [(Int, Int)] -> [Int]
offsets = tail . (fst <$>) . scanl step (0, 0)

step :: (Int, Int) -> (Int, Int) -> (Int, Int)
step (_st, ed) (sz, al) = (st', st' + sz)
	where st' = ((ed - 1) `div` al + 1) * al

alignments :: [(Int, Int)] -> Int
alignments = foldl lcm 1 . (snd <$>)

mapEntries :: Parameters -> [C.MapEntry]
mapEntries (_, ps, _, _) = (<$> ps) \(i, o, s) -> C.MapEntry {
	C.mapEntryConstantId = fromIntegral i,
	C.mapEntryOffset = fromIntegral o,
	C.mapEntrySize = fromIntegral s }

infoToCore :: forall vs a . PokableList vs =>
	HeteroParList.L vs -> (C.Info -> IO a) -> IO a
infoToCore xs f =
	allocaArray n \pmes ->
	pokeArray pmes (mapEntries ps) >>
	allocaBytesAligned tsz tal \pd ->
	pokeList pd xs >>
	f C.Info {
		C.infoMapEntryCount = fromIntegral n,
		C.infoPMapEntries = pmes,
		C.infoDataSize = fromIntegral tsz,
		C.infoPData = pd }
	where
	szals = sizeAlignments @vs
	ps@(n, _, tsz, tal) = parameters szals
