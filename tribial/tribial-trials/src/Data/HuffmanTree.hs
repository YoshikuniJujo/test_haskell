{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.HuffmanTree (
	BinTree(..), mkTr, decode1, fixedTable, fixedDstTable,

	listToMap, fixedTableList, fixedDstTableList,

	pairToCodes
	) where

import Control.Arrow
import Data.List qualified as L
import Data.Map qualified as Map

import Data.Bit (pattern O, pattern I)
import Data.Bit qualified as Bit

data BinTree a = Node (BinTree a) (BinTree a) | Leaf a deriving Show

fromList :: Show a => [([Bit.Bit], a)] -> BinTree a
fromList [([], x)] = Leaf x
fromList t = Node (fromList t1) (fromList t2)
	where
	[t1, t2] = case
		(first tail <$>) <$>
		L.groupBy (curry (uncurry (==) . ((head . fst) *** (head . fst)))) t of
		[a, b] -> [a, b]
		[x] -> error $ "fromList : single: " ++ show x
		x -> error $ "fromList : single: " ++ show x

bitListFromTo :: [Bit.Bit] -> [Bit.Bit] -> [[Bit.Bit]]
bitListFromTo b e = reverse <$> bitListFromToRv (reverse b) (reverse e)

bitListFromToRv :: [Bit.Bit] -> [Bit.Bit] -> [[Bit.Bit]]
bitListFromToRv b e
	| b == e = [b]
	| otherwise = b : bitListFromToRv (bitListNextRv b) e

bitListNextRv :: [Bit.Bit] -> [Bit.Bit]
bitListNextRv [] = []
bitListNextRv (O : bs) = (I : bs)
bitListNextRv (I : bs) = (O : bitListNextRv bs)

fixedTable :: BinTree Int
fixedTable = fromList fixedTableList

fixedTableList :: [([Bit.Bit], Int)]
fixedTableList = L.sort . (`zip` [0 ..]) $
	bitListFromTo [O, O, I, I, O, O, O, O] [I, O, I, I, I, I, I, I] ++
	bitListFromTo [I, I, O, O, I, O, O, O, O] [I, I, I, I, I, I, I, I, I] ++
	bitListFromTo [O, O, O, O, O, O, O] [O, O, I, O, I, I, I] ++
	bitListFromTo [I, I, O, O, O, O, O, O] [I, I, O, O, O, I, I, I]

decode :: BinTree a -> BinTree a -> [Bit.Bit] -> [a]
decode _ (Leaf x) [] = [x]
decode t0 (Node l _) (O : bs) = decode t0 l bs
decode t0 (Node _ r) (I : bs) = decode t0 r bs
decode t0 (Leaf x) bs = x : decode t0 t0 bs

decode1 :: BinTree a -> BinTree a -> Bit.Bit -> (Maybe a, BinTree a)
decode1 t0 (Node (Leaf x) _) O = (Just x, t0)
decode1 t0 (Node _ (Leaf x)) I = (Just x, t0)
decode1 _ (Node l _) O = (Nothing, l)
decode1 _ (Node _ r) I = (Nothing, r)

fixedDstTable :: BinTree Int
fixedDstTable = fromList fixedDstTableList

fixedDstTableList :: [([Bit.Bit], Int)]
fixedDstTableList = bitListFromTo [O, O, O, O, O] [I, I, I, I, I] `zip` [0 ..]

lenListToCodes :: Integral n => [Bit.Bit] -> [n] -> [[Bit.Bit]]
lenListToCodes _ [] = []
lenListToCodes bs (n : ns) = bs' : lenListToCodes (bitListNext bs') ns
	where
	bs' = bs ++ replicate (fromIntegral n - length bs) O

bitListNext :: [Bit.Bit] -> [Bit.Bit]
bitListNext = reverse . bitListNextRv . reverse

pairToCodes :: Integral n => [(n, a)] -> [([Bit.Bit], a)]
pairToCodes = uncurry zip . (lenListToCodes [O] `first`) . unzip

mkTr :: forall n a . (Show a, Integral n, Ord a) => [a] -> [n] -> BinTree a
mkTr xs = fromList . pairToCodes @n . L.sort . filter ((/= 0) . fst) . (`zip` xs)

listToMap :: [([Bit.Bit], Int)] -> Map.Map Int [Bit.Bit]
listToMap = Map.fromList . (uncurry (flip (,)) <$>)
