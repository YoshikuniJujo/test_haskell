{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module BottomUpMergeSort where

class Sortable s where
	empty :: s a
	add :: a -> s a -> s a
	sort :: s a -> List a

data List a = Nil | a :! !(List a) deriving Show

data ListStrict a = NilS | !a :!! !(ListStrict a) deriving Show

data SortableCollection a = SortableCollection !Int (ListStrict (List a))
	deriving Show

mrg :: Ord a => List a -> List a -> List a
mrg Nil ys = ys
mrg xs Nil = xs
mrg xa@(x :! xs) ya@(y :! ys)
	| x <= y = x :! mrg xs ya
	| otherwise = y :! mrg xa ys

addSC :: Ord a => a -> SortableCollection a -> SortableCollection a
addSC x (SortableCollection size segs) =
	SortableCollection (size + 1) (addSeg (x :! Nil) segs size)
	where
	addSeg :: Ord a => List a -> ListStrict (List a) -> Int -> ListStrict (List a)
	addSeg s ss sz = case sz `mod` 2 of
		0 -> s :!! ss
		1 -> let h :!! t = ss in addSeg (mrg s h) t (sz `div` 2)
		_ -> error "never occur"

instance Foldable ListStrict where
	foldr _ v NilS = v
	foldr (-<) v (x :!! xs) = x -< foldr (-<) v xs

sortSC :: Ord a => SortableCollection a -> List a
sortSC (SortableCollection _ segs) = foldl mrg Nil segs
