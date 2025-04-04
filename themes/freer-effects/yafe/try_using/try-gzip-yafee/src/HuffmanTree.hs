{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module HuffmanTree where

import Control.Arrow
import Data.List qualified as L

import BitArray

data BinTree a = Node (BinTree a) (BinTree a) | Leaf a deriving Show

fromList :: [([Bit], a)] -> BinTree a
fromList [([], x)] = Leaf x
fromList t = Node (fromList t1) (fromList t2)
	where
	[t1, t2] =
		(first tail <$>) <$>
		L.groupBy (curry (uncurry (==) . ((head . fst) *** (head . fst)))) t

bitListFromTo :: [Bit] -> [Bit] -> [[Bit]]
bitListFromTo b e = reverse <$> bitListFromToRv (reverse b) (reverse e)

bitListFromToRv :: [Bit] -> [Bit] -> [[Bit]]
bitListFromToRv b e
	| b == e = [b]
	| otherwise = b : bitListFromToRv (bitListNextRv b) e

bitListNextRv :: [Bit] -> [Bit]
bitListNextRv [] = []
bitListNextRv (O : bs) = (I : bs)
bitListNextRv (I : bs) = (O : bitListNextRv bs)

fixedTable :: BinTree Int
fixedTable = fromList fixedTableList

fixedTableList :: [([Bit], Int)]
fixedTableList = L.sort . (`zip` [0 ..]) $
	bitListFromTo [O, O, I, I, O, O, O, O] [I, O, I, I, I, I, I, I] ++
	bitListFromTo [I, I, O, O, I, O, O, O, O] [I, I, I, I, I, I, I, I, I] ++
	bitListFromTo [O, O, O, O, O, O, O] [O, O, I, O, I, I, I] ++
	bitListFromTo [I, I, O, O, O, O, O, O] [I, I, O, O, O, I, I, I]

decode :: BinTree a -> BinTree a -> [Bit] -> [a]
decode _ (Leaf x) [] = [x]
decode t0 (Node l _) (O : bs) = decode t0 l bs
decode t0 (Node _ r) (I : bs) = decode t0 r bs
decode t0 (Leaf x) bs = x : decode t0 t0 bs

decode1 :: BinTree a -> BinTree a -> Bit -> (Maybe a, BinTree a)
decode1 t0 (Node (Leaf x) _) O = (Just x, t0)
decode1 t0 (Node _ (Leaf x)) I = (Just x, t0)
decode1 _ (Node l _) O = (Nothing, l)
decode1 _ (Node _ r) I = (Nothing, r)

fixedDstTable :: BinTree Int
fixedDstTable = fromList fixedDstTableList

fixedDstTableList :: [([Bit], Int)]
fixedDstTableList = bitListFromTo [O, O, O, O, O] [I, I, I, I, I] `zip` [0 ..]

lenListToCodes :: Integral n => [Bit] -> [n] -> [[Bit]]
lenListToCodes _ [] = []
lenListToCodes bs (n : ns) = bs' : lenListToCodes (bitListNext bs') ns
	where
	bs' = bs ++ replicate (fromIntegral n - length bs) O

bitListNext :: [Bit] -> [Bit]
bitListNext = reverse . bitListNextRv . reverse

pairToCodes :: Integral n => [(n, a)] -> [([Bit], a)]
pairToCodes = uncurry zip . (lenListToCodes [O] `first`) . unzip
