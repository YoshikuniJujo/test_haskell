{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Sequences.ToolsYj (indexing) where

import Data.MonoTraversable qualified as Mono
import Data.Sequences qualified as Seq
import Data.Map qualified as M

indexing :: (
	Mono.MonoFoldable seq, Mono.Element seq ~ Mono.Element seq',
	Seq.IsSequence seq', Ord (Mono.Element seq),
	Seq.IsSequence seqi, Enum (Mono.Element seqi) ) => seq -> (seq', seqi)
indexing (Mono.otoList -> xs) = let
	(xs', is) = indexingGo 0 M.empty xs in
	(Seq.fromList $ M.elems xs', Seq.fromList $ toEnum <$> is)

indexingGo :: Ord a => Int -> M.Map a Int -> [a] -> (M.Map Int a, [Int])
indexingGo _ _ [] = (M.empty, [])
indexingGo idx rvdct (x : xs) = case M.lookup x rvdct of
	Nothing -> let
		(dct, is) = indexingGo (idx + 1) (M.insert x idx rvdct) xs in
		(M.insert idx x dct, idx : is)
	Just i -> let (dct, is) = indexingGo idx rvdct xs in (dct, i : is)
