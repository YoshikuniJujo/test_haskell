{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-x-partial #-}

module Data.HuffmanTree (

	-- * BIN TREE

	BinTree(..),

	-- * MAKE TREE AND TABLE

	mkTr, tableToDict,

	-- * DECODE

	decode1,

	-- * FIXED TABLE

	fixedTable, fixedDstTable,

	) where

import Control.Arrow
import Data.List qualified as L
import Data.Map qualified as Map

import Data.Bit (pattern O, pattern I)
import Data.Bit qualified as Bit

data BinTree a = Node (BinTree a) (BinTree a) | Leaf a deriving Show

fromList :: Show a => [([Bit.B], a)] -> BinTree a
fromList [([], x)] = Leaf x
fromList t = case (first tail <$>) <$>
		L.groupBy (curry (uncurry (==) . ((head . fst) *** (head . fst)))) t of
	[t1, t2] -> Node (fromList t1) (fromList t2)
	[x] -> error $ "fromList : single: " ++ show x
	x -> error $ "fromList : single: " ++ show x

bitListFromTo :: [Bit.B] -> [Bit.B] -> [[Bit.B]]
bitListFromTo b e = reverse <$> bitListFromToRv (reverse b) (reverse e)

bitListFromToRv :: [Bit.B] -> [Bit.B] -> [[Bit.B]]
bitListFromToRv b e
	| b == e = [b]
	| otherwise = b : bitListFromToRv (bitListNextRv b) e

bitListNextRv :: [Bit.B] -> [Bit.B]
bitListNextRv [] = []
bitListNextRv (O : bs) = (I : bs)
bitListNextRv (I : bs) = (O : bitListNextRv bs)

fixedTable :: BinTree Int
fixedTable = fromList fixedTableList

fixedTableList :: [([Bit.B], Int)]
fixedTableList = L.sort . (`zip` [0 ..]) $
	bitListFromTo [O, O, I, I, O, O, O, O] [I, O, I, I, I, I, I, I] ++
	bitListFromTo [I, I, O, O, I, O, O, O, O] [I, I, I, I, I, I, I, I, I] ++
	bitListFromTo [O, O, O, O, O, O, O] [O, O, I, O, I, I, I] ++
	bitListFromTo [I, I, O, O, O, O, O, O] [I, I, O, O, O, I, I, I]

decode1 :: BinTree a -> BinTree a -> Bit.B -> (Maybe a, BinTree a)
decode1 t0 (Node (Leaf x) _) O = (Just x, t0)
decode1 t0 (Node _ (Leaf x)) I = (Just x, t0)
decode1 _ (Node l _) O = (Nothing, l)
decode1 _ (Node _ r) I = (Nothing, r)
decode1 _ _ _ = error "bad"

fixedDstTable :: BinTree Int
fixedDstTable = fromList fixedDstTableList

fixedDstTableList :: [([Bit.B], Int)]
fixedDstTableList = bitListFromTo [O, O, O, O, O] [I, I, I, I, I] `zip` [0 ..]

lenListToCodes :: Integral n => [Bit.B] -> [n] -> [[Bit.B]]
lenListToCodes _ [] = []
lenListToCodes bs (n : ns) = bs' : lenListToCodes (bitListNext bs') ns
	where
	bs' = bs ++ replicate (fromIntegral n - length bs) O

bitListNext :: [Bit.B] -> [Bit.B]
bitListNext = reverse . bitListNextRv . reverse

tableToDict :: (Ord a, Integral n) => Map.Map a n -> Map.Map a [Bit.B]
tableToDict = pairToCodes'

pairToCodes' :: (Ord a, Integral n) => Map.Map a n -> Map.Map a [Bit.B]
pairToCodes' = Map.fromList
	. ((uncurry $ flip (,)) <$>) . pairToCodes . ((uncurry $ flip (,)) <$>)
	. L.sortOn snd . Map.toList

pairToCodes :: Integral n => [(n, a)] -> [([Bit.B], a)]
pairToCodes = uncurry zip . (lenListToCodes [O] `first`) . unzip

mkTr :: forall n a . (Show a, Integral n, Ord a) => [a] -> [n] -> BinTree a
mkTr xs = fromList . pairToCodes @n . L.sort . filter ((/= 0) . fst) . (`zip` xs)
