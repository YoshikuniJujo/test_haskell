{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-x-partial #-}

module Data.Huffman (BinTree, decode1, makeTree, tableToDict) where

import Control.Arrow
import Data.List qualified as L
import Data.Map qualified as Map
import Data.ByteString.Bit (pattern O, pattern I)
import Data.ByteString.Bit qualified as Bit

data BinTree a = Leaf a | Node (BinTree a) (BinTree a) deriving Show

decode1 :: BinTree a -> BinTree a -> Bit.B -> (Maybe a, BinTree a)
decode1 t0 (Node (Leaf x) _) O = (Just x, t0)
decode1 t0 (Node _ (Leaf x)) I = (Just x, t0)
decode1 _ (Node l _) O = (Nothing, l)
decode1 _ (Node _ r) I = (Nothing, r)
decode1 _ (Leaf _) _ = error "bad"

makeTree :: (Integral n, Ord a) => [a] -> [n] -> BinTree a
makeTree xs = fromList . pairToCodes . L.sort . filter ((/= 0) . fst) . (`zip` xs)

fromList :: [([Bit.B], a)] -> BinTree a
fromList [([], x)] = Leaf x
fromList t = case ((first tail <$>) <$>) . (`L.groupBy` t)
		. curry . (uncurry (==) .) $ head . fst *** head . fst of
	[t1, t2] -> fromList t1 `Node` fromList t2
	_ -> error "fromList: bad "

tableToDict :: (Ord a, Integral n) => Map.Map a n -> Map.Map a [Bit.B]
tableToDict = Map.fromList
	. ((uncurry $ flip (,)) <$>) . pairToCodes . ((uncurry $ flip (,)) <$>)
	. L.sortOn snd . Map.toList

pairToCodes :: Integral n => [(n, a)] -> [([Bit.B], a)]
pairToCodes = uncurry zip . (lenListToCodes `first`) . unzip

lenListToCodes :: Integral n => [n] -> [[Bit.B]]
lenListToCodes = go [O]
	where go bs = \case
		[] -> []
		n : ns -> bs' : go (bitListNext bs') ns
			where
			bs' = bs ++ replicate (fromIntegral n - length bs) O

bitListNext :: [Bit.B] -> [Bit.B]
bitListNext = reverse . go . reverse
	where go = \case [] -> []; O : bs -> (I : bs); I : bs -> (O : go bs)
