{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Specialization.Middle.Internal where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Control.Monad.Cont
import Data.HeteroList hiding (length)

import qualified Data.HeteroList as HList

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

class HList.StorableList vs => StoreValues vs where storeValues :: Ptr () -> vs -> IO ()

instance StoreValues () where storeValues _ _ = pure ()
instance StoreValues Tip where storeValues _ _ = pure ()

instance (Storable v, StoreValues vs) => StoreValues (v :.: vs) where
	storeValues p (x :.: xs) = do
		let	p' = castPtr . alignPtr p $ alignment x
		poke p' x
		storeValues (p' `plusPtr` sizeOf x) xs

infoToCore :: StoreValues vs => vs -> ContT r IO C.Info
infoToCore xs = do
	pmes <- ContT $ allocaArray n
	lift . pokeArray pmes $ mapEntries ps
	pd <- ContT $ allocaBytesAligned tsz tal
	lift $ storeValues pd xs
	pure C.Info {
		C.infoMapEntryCount = fromIntegral n,
		C.infoPMapEntries = pmes,
		C.infoDataSize = fromIntegral tsz,
		C.infoPData = pd }
	where
	szals = HList.sizeAlignments xs
	ps@(n, _, tsz, tal) = parameters szals

infoToCore' :: (StorableList' vs, StoreHetero' vs) => HeteroList' vs -> ContT r IO C.Info
infoToCore' xs = do
	pmes <- ContT $ allocaArray n
	lift . pokeArray pmes $ mapEntries ps
	pd <- ContT $ allocaBytesAligned tsz tal
	lift $ storeHetero' pd xs
	pure C.Info {
		C.infoMapEntryCount = fromIntegral n,
		C.infoPMapEntries = pmes,
		C.infoDataSize = fromIntegral tsz,
		C.infoPData = pd }
	where
	szals = HList.sizeAlignments' xs
	ps@(n, _, tsz, tal) = parameters szals
