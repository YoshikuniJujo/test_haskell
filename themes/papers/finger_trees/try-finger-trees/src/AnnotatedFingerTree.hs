{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}
{-# LANGUAGE GADTs, DataKinds, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=GHC.TypeLits.Normalise #-}

module AnnotatedFingerTree where

import GHC.TypeLits

import Range

reducer :: Foldable t => (a -> b -> b) -> t a -> b -> b
reducer = flip . foldr

reducel :: Foldable t => (b -> a -> b) -> b -> t a -> b
reducel = foldl

class Monoid v => Measured a v where
	measure :: a -> v

data Node v a = Node v (Range 2 3 a) deriving Show

node2 :: Measured a v => a -> a -> Node v a
node2 a b = Node (measure a <> measure b) $ a :. b :. Nil

node3 :: Measured a v => a -> a -> a -> Node v a
node3 a b c = Node (measure a <> measure b <> measure c) $ a :. b :. c :.. Nil

instance Foldable (Node v) where
	foldr (-<) z (Node _ xs) = foldr (-<) z xs
	foldl (>-) z (Node _ xs) = foldl (>-) z xs

instance Monoid v => Measured (Node v a) v where
	measure (Node v _) = v

type Digit = Range 1 4

instance Measured a v => Measured (Digit a) v where
	measure = reducel (\i a -> i <> measure a) mempty

data FingerTree v a
	= Empty
	| Single a
	| Deep v (Digit a) (FingerTree v (Node v a)) (Digit a)
	deriving Show

deep :: Measured a v =>
	Digit a -> FingerTree v (Node v a) -> Digit a -> FingerTree v a
deep pr m sf = Deep (measure pr <> measure m <> measure sf) pr m sf

instance Measured a v => Measured (FingerTree v a) v where
	measure Empty = mempty
	measure (Single x) = measure x
	measure (Deep v _ _ _) = v

instance Foldable (FingerTree v) where
	foldr :: forall a b . (a -> b -> b) -> b -> FingerTree v a -> b
	foldr _ z Empty = z
	foldr (-<) z (Single x) = x -< z
	foldr (-<) z (Deep _ pr m sf) = pr -<. (m -<.. (sf -<. z))
		where
		(-<.) :: forall t . Foldable t => t a -> b -> b
		(-<.) = reducer (-<)
		(-<..) :: forall t t' . (Foldable t, Foldable t') => t (t' a) -> b -> b
		(-<..) = reducer (-<.)
	foldl :: forall a b . (b -> a -> b) -> b -> FingerTree v a -> b
	foldl _ z Empty = z
	foldl (>-) z (Single x) = z >- x
	foldl (>-) z (Deep _ pr m sf) = ((z >-. pr) >-.. m) >-. sf
		where
		(>-.) :: forall t . Foldable t => b -> t a -> b
		(>-.) = reducel (>-)
		(>-..) :: forall t t' . (Foldable t, Foldable t') => b -> t (t' a) -> b
		(>-..) = reducel (>-.)

infixr 5 <||, <|, <|.

(<||) :: Measured a v => a -> Digit a -> Either (Digit a) (Digit a, Node v a)
a <|| b :. Nil = Left $ a :. b :.. Nil
a <|| b :. c :.. Nil = Left $ a :. b :.. c :.. Nil
a <|| b :. c :.. d :.. Nil = Left $ a :. b :.. c :.. d :.. Nil
a <|| b :. c :.. d :.. e :.. Nil = Right (a :. b :.. Nil, node3 c d e)
_ <|| _ = error "never occur"

(<|) :: Measured a v => a -> FingerTree v a -> FingerTree v a
a <| Empty = Single a
a <| Single b = deep (a :. Nil) Empty (b :. Nil)
a <| Deep _ pr m sf = case a <|| pr of
	Left pr' -> deep pr' m sf
	Right (pr', n3) -> deep pr' (n3 <| m) sf

(<|.) :: (Measured a v, Foldable t) => t a -> FingerTree v a -> FingerTree v a
(<|.) = reducer (<|)

toTree :: (Measured a v, Foldable t) => t a -> FingerTree v a
toTree = (<|. Empty)

infixl 5 ||>, |>, |>.

(||>) :: Measured a v => Digit a -> a -> Either (Digit a) (Node v a, Digit a)
a :. Nil ||> b = Left $ a :. b :.. Nil
a :. b :.. Nil ||> c = Left $ a :. b :.. c :.. Nil
a :. b :.. c :.. Nil ||> d = Left $ a :. b :.. c :.. d :.. Nil
a :. b :.. c :.. d :.. Nil ||> e = Right (node3 a b c, d :. e :.. Nil)
_ ||> _ = error "never occur"

(|>) :: Measured a v => FingerTree v a -> a -> FingerTree v a
Empty |> a = Single a
Single a |> b = deep (a :. Nil) Empty (b :. Nil)
Deep _ pr m sf |> a = case sf ||> a of
	Left sf' -> deep pr m sf'
	Right (n3, sf') -> deep pr (m |> n3) sf'

(|>.) :: (Measured a v, Foldable t) => FingerTree v a -> t a -> FingerTree v a
(|>.) = reducel (|>)

data ViewL s a = NilL | ConsL a (s a) deriving Show

viewL :: Measured a v => FingerTree v a -> ViewL (FingerTree v) a
viewL Empty = NilL
viewL (Single x) = ConsL x Empty
viewL (Deep _ (a :. pr') m sf) = ConsL a $ deepL pr' m sf

nodeToDigit :: Node v a -> Digit a
nodeToDigit (Node _ xs) = loosen xs

deepL :: Measured a v =>
	Range 0 3 a -> FingerTree v (Node v a) -> Digit a -> FingerTree v a
deepL Nil m sf = case viewL m of
	NilL -> toTree sf
	ConsL a m' -> deep (nodeToDigit a) m' sf
deepL (a :.. pr) m sf = deep (loosen $ a :. pr) m sf
deepL _ _ _ = error "never occur"

newtype Size = Size { getSize :: Int } deriving (Show, Eq, Ord)
instance Semigroup Size where Size m <> Size n = Size $ m + n
instance Monoid Size where mempty = Size 0

newtype Elem a = Elem { getElem :: a } deriving Show
instance Measured (Elem a) Size where measure _ = Size 1

sampleList :: [Int]
sampleList = [1 .. 7]

sampleFTree :: FingerTree Size (Elem Int)
sampleFTree = toTree $ Elem <$> sampleList

data ViewR s a = NilR | ConsR (s a) a deriving Show

viewR :: Measured a v => FingerTree v a -> ViewR (FingerTree v) a
viewR Empty = NilR
viewR (Single x) = ConsR Empty x
viewR (Deep _ pr m sf) = case unsnoc sf of
	(sf', a) -> ConsR (deepR pr m sf') a

deepR :: Measured a v =>
	Digit a -> FingerTree v (Node v a) -> Range 0 3 a -> FingerTree v a
deepR pr m Nil = case viewR m of
	NilR -> toTree pr
	ConsR m' a -> deep pr m' (nodeToDigit a)
deepR pr m (a :.. sf) = deep pr m (loosen $ a :. sf)
deepR _ _ _ = error "never occur"

app3 :: forall a v . Measured a v =>
	FingerTree v a -> Range 0 4 a -> FingerTree v a -> FingerTree v a
app3 Empty ts ys = ts <|. ys
app3 xs ts Empty = xs |>. ts
app3 (Single x) ts ys = x <| (ts <|. ys)
app3 xs ts (Single y) = (xs |>. ts) |> y
app3 (Deep _ pr1 m1 sf1) ts (Deep _ pr2 m2 sf2) =
	deep pr1 (app3 m1 (loosen (nodes (sf1 ++. ts ++.. pr2) :: Range 1 4 (Node v a))) m2) sf2
--	deep pr1 (app3 m1 (loosen (nodes (sf1 ++. ts ++.. pr2))) m2) sf2

nodes0 :: Measured a v => Range 2 6 a -> Range 1 2 (Node v a)
nodes0 (a :. b :. Nil) = node2 a b :. Nil
nodes0 (a :. b :. c :.. Nil) = node3 a b c :. Nil
nodes0 (a :. b :. c :.. d :.. Nil) = node2 a b :. node2 c d :.. Nil
nodes0 (a :. b :. c :.. d :.. e :.. Nil) = node3 a b c :. node2 d e :.. Nil
nodes0 (a :. b :. c :.. d :.. e :.. f :.. Nil) = node3 a b c :. node3 d e f :.. Nil
nodes0 _ = error "never occur"

class Nodes m m' where
	nodes :: Measured a v => Range 2 m a -> Range 1 m' (Node v a)

instance Nodes 6 2 where
	nodes = nodes0

instance {-# OVERLAPPABLE #-} Nodes (m - 3) (m' - 1) => Nodes m m' where
	nodes :: forall a v . Measured a v => Range 2 m a -> Range 1 m' (Node v a)
	nodes (a :. b :. Nil) = node2 a b :. Nil
	nodes (a :. b :. c :.. Nil) = node3 a b c :. Nil
	nodes (a :. b :. c :.. d :.. Nil) = node2 a b :. node2 c d :.. Nil
	nodes (a :. b :. c :.. d :.. e :.. xs) = node3 a b c .:.. (nodes (d :. e :. xs :: Range 2 (m - 3) a) :: Range 1 (m' - 1) (Node v a))
--	nodes (a :. b :. c :.. d :.. e :.. xs) = node3 a b c .:.. (nodes (d :. e :. xs) :: Range 1 (m' - 1) (Node v a))
	nodes _ = error "never occur"

(><) :: Measured a v => FingerTree v a -> FingerTree v a -> FingerTree v a
xs >< ys = app3 xs Nil ys

data Split f a = Split (f a) a (f a) deriving Show

class SplitDigit m where
	splitDigit :: Measured a v => (v -> Bool) -> v -> Range 1 m a -> Split (Range 0 (m - 1)) a

instance SplitDigit 1 where
	splitDigit _ _ (a :. Nil) = Split Nil a Nil
	splitDigit _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-} (2 <= m, SplitDigit (m - 1)) => SplitDigit m where
	splitDigit :: forall a v . Measured a v => (v -> Bool) -> v -> Range 1 m a -> Split (Range 0 (m - 1)) a
	splitDigit _ _ (a :. Nil) = Split Nil a Nil
	splitDigit p i (a :. b :.. as :: Range 1 m a)
		| p i' = Split Nil a (loosenMax (b :.. as :: 2 <= m => Range 0 (m - 1) a) :: Range 0 (m - 1) a)
		| otherwise = let Split l x r = splitDigit p i' (b :. as) in Split (a :.. l) x (loosenMax (r :: Range 0 (m - 2) a) :: Range 0 (m - 1) a)
		where i' = i <> measure a
	splitDigit _ _ _ = error "never occur"

{-
splitDigit :: Measured a v => (v -> Bool) -> v -> Digit a -> Split (Range 0 3) a
splitDigit _ _ (a :. Nil) = Split Nil a Nil
splitDigit p i (a :. as)
	| p i' = Split Nil a as
	| otherwise = let Split l x r = splitDigit p i' as in Split (a :.. l) x r
	where i' = i <> measure a
	-}

splitTree :: Measured a v =>
	(v -> Bool) -> v -> FingerTree v a -> Split (FingerTree v) a
splitTree _ _ Empty = error "can't split"
splitTree _ _ (Single x) = Split Empty x Empty
splitTree p i (Deep _ pr m sf)
	| p vpr = let Split l x r = splitDigit p i pr in
		Split (toTree l) x (deepL r m sf)
	| p vm = let
		Split ml xs mr = splitTree p vpr m
		Split l x r = splitDigit p (vpr <> measure ml) (nodeToDigit xs) in
		Split (deepR pr ml l) x (deepL r mr sf)
	| otherwise = let Split l x r = splitDigit p vm sf in
		Split (deepR pr m l) x (toTree r)
	where
	vpr = i <> measure pr
	vm = vpr <> measure m

split :: Measured a v =>
	(v -> Bool) -> FingerTree v a -> (FingerTree v a, FingerTree v a)
split _ Empty = (Empty, Empty)
split p xs
	| p $ measure xs = (l, x <| r)
	| otherwise = (xs, Empty)
	where Split l x r = splitTree p mempty xs

takeUntil, dropUntil :: Measured a v => (v -> Bool) -> FingerTree v a -> FingerTree v a
takeUntil p = fst . split p
dropUntil p = snd . split p

newtype Seq a = Seq (FingerTree Size (Elem a)) deriving Show

length :: Seq a -> Int
length (Seq xs) = getSize $ measure xs

splitAt :: Int -> Seq a -> (Seq a, Seq a)
splitAt i (Seq xs) = (Seq l, Seq r)
	where (l, r) = split (Size i <) xs

(!) :: Seq a -> Int -> a
Seq xs ! i = getElem x
	where Split _ x _ = splitTree (Size i <) (Size 0) xs

addToSeq :: Foldable t => t a -> Seq a -> Seq a
addToSeq ts (Seq ft) = Seq $ reducer ((<|) . Elem) ts ft

toSeq :: Foldable t => t a -> Seq a
toSeq = (`addToSeq` Seq Empty)

data Prio a = MInfty | Prio a deriving (Show, Eq, Ord)

instance Ord a => Semigroup (Prio a) where
	MInfty <> p = p
	p <> MInfty = p
	Prio m <> Prio n = Prio $ m `max` n

instance Ord a => Monoid (Prio a) where
	mempty = MInfty

newtype PQueue a = PQueue (FingerTree (Prio a) (Elem a)) deriving Show

instance Ord a => Measured (Elem a) (Prio a) where
	measure (Elem x) = Prio x

extractMax :: Ord a => PQueue a -> (a, PQueue a)
extractMax (PQueue q) = (x, PQueue $ l >< r)
	where
	Split l (Elem x) r = splitTree (measure q <=) mempty q

addToPQueue :: (Ord a, Foldable t) => t a -> PQueue a -> PQueue a
addToPQueue ts (PQueue ft) = PQueue $ reducer ((<|) . Elem) ts ft

toPQueue :: (Ord a, Foldable t) => t a -> PQueue a
toPQueue = (`addToPQueue` PQueue Empty)

data Key a = NoKey | Key a deriving (Show, Eq, Ord)

instance Semigroup (Key a) where
	k <> NoKey = k
	_ <> k = k

instance Monoid (Key a) where
	mempty = NoKey

newtype OrdSeq a = OrdSeq (FingerTree (Key a) (Elem a)) deriving Show

instance Measured (Elem a) (Key a) where
	measure (Elem x) = Key x

partition :: (Ord a) => a -> OrdSeq a -> (OrdSeq a, OrdSeq a)
partition k (OrdSeq xs) = (OrdSeq l, OrdSeq r)
	where (l, r) = split (>= Key k) xs

insert :: Ord a => a -> OrdSeq a -> OrdSeq a
insert x (OrdSeq xs) = OrdSeq (l >< (Elem x <| r))
	where (l, r) = split (>= Key x) xs

deleteAll :: Ord a => a -> OrdSeq a -> OrdSeq a
deleteAll x (OrdSeq xs) = OrdSeq (l >< r')
	where
	(l, r) = split (>= Key x) xs
	(_, r') = split (> Key x) r

merge :: Ord a => OrdSeq a -> OrdSeq a -> OrdSeq a
merge (OrdSeq xs) (OrdSeq ys) = OrdSeq (merge' xs ys)
	where
	merge' as bs = case viewL bs of
		NilL -> as
		ConsL a bs' -> l >< (a <| merge' bs' r)
			where (l, r) = split (> measure a) as

addToOrdSeq :: (Ord a, Foldable t) => t a -> OrdSeq a -> OrdSeq a
addToOrdSeq = reducer insert

toOrdSeq :: (Ord a, Foldable t) => t a -> OrdSeq a
toOrdSeq = (`addToOrdSeq` OrdSeq Empty)

fromOrdSeq :: OrdSeq a -> [a]
fromOrdSeq (OrdSeq ft) = foldr ((:) . getElem) [] ft

data Interval = Interval { low :: Double, high :: Double } deriving Show

newtype IntervalTree = IntervalTree (FingerTree (Key Double, Prio Double) Interval) deriving Show

instance Measured Interval (Key Double, Prio Double) where
	measure i = (Key (low i), Prio (high i))

insertIntervalTree :: Interval -> IntervalTree -> IntervalTree
insertIntervalTree i (IntervalTree xs) = IntervalTree (l >< (i <| r))
	where (l, r) = split ((>= Key (low i)) . fst) xs

addToIntervalTree :: Foldable t => t (Double, Double) -> IntervalTree -> IntervalTree
addToIntervalTree = reducer (insertIntervalTree . uncurry Interval)

toIntervalTree :: Foldable t => t (Double, Double) -> IntervalTree
toIntervalTree = (`addToIntervalTree` IntervalTree Empty)

atleast, greater :: Double -> (Key Double, Prio Double) -> Bool
atleast k (_, n) = Prio k <= n
greater k (n, _) = n > Key k

intervalSearch :: IntervalTree -> Interval -> Maybe Interval
intervalSearch (IntervalTree t) i
	| atleast (low i) (measure t) && low x <= high i = Just x
	| otherwise = Nothing
	where Split _ x _ = splitTree (atleast (low i)) mempty t

intervalMatch :: IntervalTree -> Interval -> [Interval]
intervalMatch (IntervalTree t) i = matches (takeUntil (greater (high i)) t)
	where
	matches xs = case viewL (dropUntil (atleast (low i)) xs) of
		NilL -> []
		ConsL x xs' -> x : matches xs'
