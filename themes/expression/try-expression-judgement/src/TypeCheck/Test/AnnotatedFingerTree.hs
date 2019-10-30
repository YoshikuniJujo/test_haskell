{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=TypeCheck.Nat #-}

module TypeCheck.Test.AnnotatedFingerTree where

import Control.Arrow

import GHC.TypeLits

import TypeCheck.Test.Range

reducer :: Foldable t => (a -> b -> b) -> t a -> b -> b
reducer = flip . foldr

reducel :: Foldable t => (b -> a -> b) -> b -> t a -> b
reducel = foldl

class Monoid v => Measured a v where measure :: a -> v

data Node v a = Node v (RangeL 2 3 a) deriving Show

node2 :: Measured a v => a -> a -> Node v a
node2 a b = Node (measure a <> measure b) $ a :. b :. NilL

node3 :: Measured a v => a -> a -> a -> Node v a
node3 a b c = Node (measure a <> measure b <> measure c) $ a :. b :. c :.. NilL

instance Foldable (Node v) where
	foldr (-<) z (Node _ xs) = foldr (-<) z xs
	foldl (>-) z (Node _ xs) = foldl (>-) z xs

instance Monoid v => Measured (Node v a) v where measure (Node v _) = v

type DigitL = RangeL 1 4
type DigitR = RangeR 1 4

instance Measured a v => Measured (DigitL a) v where
	measure = reducel (\i a -> i <> measure a) mempty

instance Measured a v => Measured (DigitR a) v where
	measure = reducel (\i a -> i <> measure a) mempty

data FingerTree v a
	= Empty
	| Single a
	| Deep v (DigitL a) (FingerTree v (Node v a)) (DigitR a)
	deriving Show

deep :: Measured a v =>
	DigitL a -> FingerTree v (Node v a) -> DigitR a -> FingerTree v a
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

(<||) :: Measured a v => a -> DigitL a -> Either (DigitL a) (DigitL a, Node v a)
a <|| b :. NilL = Left $ a :. b :.. NilL
a <|| b :. c :.. NilL = Left $ a :. b :.. c :.. NilL
a <|| b :. c :.. d :.. NilL = Left $ a :. b :.. c :.. d :.. NilL
a <|| b :. c :.. d :.. e :.. NilL = Right (a :. b :.. NilL, node3 c d e)
_ <|| _ = error "never occur"

(<|) :: Measured a v => a -> FingerTree v a -> FingerTree v a
a <| Empty = Single a
a <| Single b = deep (a :. NilL) Empty (NilR :+ b)
a <| Deep _ pr m sf = case a <|| pr of
	Left pr' -> deep pr' m sf
	Right (pr', n3) -> deep pr' (n3 <| m) sf

(<|.) :: (Measured a v, Foldable t) => t a -> FingerTree v a -> FingerTree v a
(<|.) = reducer (<|)

toTree :: (Measured a v, Foldable t) => t a -> FingerTree v a
toTree = (<|. Empty)

instance Measured Char () where measure _ = ()

infixl 5 ||>, |>, |>.

(||>) :: Measured a v => DigitR a -> a -> Either (DigitR a) (Node v a, DigitR a)
NilR :+ a ||> b = Left $ NilR :++ a :+ b
NilR :++ a :+ b ||> c = Left $ NilR :++ a :++ b :+ c
NilR :++ a :++ b :+ c ||> d = Left $ NilR :++ a :++ b :++ c :+ d
NilR :++ a :++ b :++ c :+ d ||> e = Right (node3 a b c, NilR :++ d :+ e)
_ ||> _ = error "never occur"

(|>) :: Measured a v => FingerTree v a -> a -> FingerTree v a
Empty |> a = Single a
Single a |> b = deep (a :. NilL) Empty (NilR :+ b)
Deep _ pr m sf |> a = case sf ||> a of
	Left sf' -> deep pr m sf'
	Right (n3, sf') -> deep pr (m |> n3) sf'

(|>.) :: (Measured a v, Foldable t) => FingerTree v a -> t a -> FingerTree v a
(|>.) = reducel (|>)

data ViewL s a = NL | ConsL a (s a) deriving Show

viewL :: Measured a v => FingerTree v a -> ViewL (FingerTree v) a
viewL Empty = NL
viewL (Single x) = ConsL x Empty
viewL (Deep _ (a :. pr') m sf) = ConsL a $ deepL pr' m sf

nodeToDigitL :: Node v a -> DigitL a
nodeToDigitL (Node _ xs) = loosenL xs

deepL :: Measured a v =>
	RangeL 0 3 a -> FingerTree v (Node v a) -> DigitR a -> FingerTree v a
deepL NilL m sf = case viewL m of
	NL -> toTree sf
	ConsL a m' -> deep (nodeToDigitL a) m' sf
deepL (a :.. pr) m sf = deep (loosenL $ a :. pr) m sf
deepL _ _ _ = error "never occur"

sampleHello :: FingerTree () Char
sampleHello = toTree "Hello, world!"

newtype Size = Size { getSize :: Int } deriving (Show, Eq, Ord)
instance Semigroup Size where Size m <> Size n = Size $ m + n
instance Monoid Size where mempty = Size 0

newtype Elem a = Elem { getElem :: a } deriving Show
instance Measured (Elem a) Size where measure _ = Size 1

sampleTree ::  FingerTree Size (Elem Int)
sampleTree = toTree $ Elem <$> [1 .. 7]

data ViewR s a = NR | ConsR (s a) a deriving Show

viewR :: Measured a v => FingerTree v a -> ViewR (FingerTree v) a
viewR Empty = NR
viewR (Single x) = ConsR Empty x
viewR (Deep _ pr m (sf' :+ a)) = ConsR (deepR pr m sf') a

nodeToDigitR :: Node v a -> DigitR a
nodeToDigitR (Node _ xs) = loosenR $ leftToRight xs

deepR :: Measured a v =>
	DigitL a -> FingerTree v (Node v a) -> RangeR 0 3 a -> FingerTree v a
deepR pr m NilR = case viewR m of
	NR -> toTree pr
	ConsR m' a -> deep pr m' (nodeToDigitR a)
deepR pr m (sf :++ a) = deep pr m (loosenR $ sf :+ a)
deepR _ _ _ = error "never occur"

app3 :: forall v a . Measured a v => FingerTree v a -> RangeL 0 4 a -> FingerTree v a -> FingerTree v a
app3 Empty ts xs = ts <|. xs
app3 xs ts Empty = xs |>. ts
app3 (Single x) ts xs = x <| (ts <|. xs)
app3 xs ts (Single x) = (xs |>. ts) |> x
app3 (Deep _ pr1 m1 sf1) ts (Deep _ pr2 m2 sf2) =
	deep pr1 (app3 m1 (loosenL (nodes (rightToLeft sf1 ++. ts ++. pr2) :: RangeL 1 4 (Node v a))) m2) sf2

class Nodes m m' where
	nodes :: Measured a v => RangeL 2 m a -> RangeL 1 m' (Node v a)

instance Nodes 3 1 where
	nodes (a :. b :. NilL) = node2 a b :. NilL
	nodes (a :. b :. c :.. NilL) = node3 a b c :. NilL
	nodes _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	(1 <= m' - 1, 1 <= m', Nodes (m - 3) (m' - 1)) => Nodes m m' where
	nodes :: forall a v . Measured a v => RangeL 2 m a -> RangeL 1 m' (Node v a)
	nodes (a :. b :. NilL) = node2 a b :. NilL
	nodes (a :. b :. c :.. NilL) = node3 a b c :. NilL
	nodes (a :. b :. c :.. d :.. NilL) = node2 a b :. node2 c d :.. NilL
	nodes (a :. b :. c :.. d :.. e :.. xs) = node3 a b c .:.. (nodes (d :. e :. xs :: RangeL 2 (m - 3) a) :: RangeL 1 (m' - 1) (Node v a))
	nodes _ = error "never occur"

(><) :: Measured a v => FingerTree v a -> FingerTree v a -> FingerTree v a
xs >< ys = app3 xs NilL ys

data Split f a = Split (f a) a (f a) deriving Show

class SplitDigitL m where
	splitDigitL :: Measured a v => (v -> Bool) -> v -> RangeL 1 m a -> Split (RangeL 0 (m - 1)) a

instance SplitDigitL 1 where
	splitDigitL _ _ (a :. NilL) = Split NilL a NilL
	splitDigitL _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-} (LoosenLMax 0 (m - 2) (m - 1), SplitDigitL (m - 1)) => SplitDigitL m where
	splitDigitL :: forall a v . Measured a v => (v -> Bool) -> v -> RangeL 1 m a -> Split (RangeL 0 (m - 1)) a
	splitDigitL _ _ (a :. NilL) = Split NilL a NilL
	splitDigitL p i (a :. b :.. as :: RangeL 1 m a)
		| p i' = Split NilL a (b :.. as)
		| otherwise = let
			Split l x r = splitDigitL p i' (b :. as) in
			Split (a :.. l) x (loosenLMax (r :: RangeL 0 (m - 2) a) :: RangeL 0 (m - 1) a)
		where i' = i <> measure a
	splitDigitL _ _ _ = error "never occur"

class SplitDigitR m where
	splitDigitRGen :: Measured a v => (v -> Bool) -> v -> RangeR 1 m a -> Either v (Split (RangeR 0 (m - 1)) a)

instance SplitDigitR 1 where
	splitDigitRGen p i (NilR :+ a)
		| p i' = Right $ Split NilR a NilR
		| otherwise = Left i'
		where i' = i <> measure a
	splitDigitRGen _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-} (LoosenRMax 0 (m - 2) (m - 1), SplitDigitR (m - 1)) => SplitDigitR m where
	splitDigitRGen :: forall a v . Measured a v =>
		(v -> Bool) -> v -> RangeR 1 m a -> Either v (Split (RangeR 0 (m - 1)) a)
	splitDigitRGen p i (NilR :+ a)
		| p i' = Right $ Split NilR a NilR
		| otherwise = Left i'
		where i' = i <> measure a
	splitDigitRGen p i (as :++ a :+ b) = case splitDigitRGen p i $ as :+ a of
		Left i'	| p i'' -> Right $ Split (as :++ a) b NilR
			| otherwise -> Left i''
			where i'' = i' <> measure b
		Right (Split l x r) -> Right $ Split (loosenRMax (l :: RangeR 0 (m - 2) a) :: RangeR 0 (m - 1) a) x (r :++ b)
	splitDigitRGen _ _ _ = error "never occur"

splitDigitR :: (Measured a v, SplitDigitR m) => (v -> Bool) -> v -> RangeR 1 m a -> Split (RangeR 0 (m - 1)) a
splitDigitR _ _ (NilR :+ a) = Split NilR a NilR
splitDigitR p i aa@(as :+ a) = case splitDigitRGen p i aa of
	Left _ -> Split as a NilR
	Right s -> s

splitTree :: Measured a v =>
	(v -> Bool) -> v -> FingerTree v a -> Split (FingerTree v) a
splitTree _ _ Empty = error "can't split"
splitTree _ _ (Single x) = Split Empty x Empty
splitTree p i (Deep _ pr m sf)
	| p vpr = let Split l x r = splitDigitL p i pr in
		Split (toTree l) x (deepL r m sf)
	| p vm = let
		Split ml xs mr = splitTree p vpr m
		Split l x r = splitDigitL p (vpr <> measure ml) (nodeToDigitL xs) in
		Split (deepR pr ml (leftToRight l)) x (deepL r mr sf)
	| otherwise = let Split l x r = splitDigitR p vm sf in
		Split (deepR pr m l) x (toTree r)
	where
	vpr = i <> measure pr
	vm = vpr <> measure m

trySomeL :: Int -> Int -> [Int] -> ([Int], [Int])
trySomeL _ _ [] = ([], [])
trySomeL n i (a : as)
	| i' >= n = ([a], as)
	| otherwise = (a :) `first` trySomeL n i' as
	where i' = i + a

trySomeR :: Int -> Int -> [Int] -> (([Int], [Int]), Maybe Int)
trySomeR _ i [] = (([], []), Just i)
trySomeR n i (a : as)
	| Just i' <- mi', i' < n = ((a :) `second` r, Just $ i' + a)
	| otherwise = ((a :) `first` r, Nothing)
	where (r, mi') = trySomeR n i as

trySomeR' :: Int -> Int -> [Int] -> Either ([Int], Int) ([Int], [Int])
trySomeR' _ i [] = Left ([], i)
trySomeR' n i (a : as) = case trySomeR' n i as of
	Left (bs, i')
		| i' >= n -> Right ([], bs)
		| otherwise -> Left (a : bs, i' + a)
	Right (l, r) -> Right (a : l, r)

split :: Measured a v =>
	(v -> Bool) -> FingerTree v a -> (FingerTree v a, FingerTree v a)
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

addToSeq' :: Foldable t => Seq a -> t a -> Seq a
addToSeq' (Seq ft) ts = Seq $ reducel (\f x -> f |> Elem x) ft ts

toSeq :: Foldable t => t a -> Seq a
toSeq = (`addToSeq` Seq Empty)

data Prio a = MInfty | Prio a deriving (Show, Eq, Ord)

instance Ord a => Semigroup (Prio a) where
	MInfty <> p = p
	p <> MInfty = p
	Prio m <> Prio n = Prio $ m `max` n

instance Ord a => Monoid (Prio a) where mempty = MInfty

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

instance Semigroup (Key a) where k <> NoKey = k; _ <> k = k
instance Monoid (Key a) where mempty = NoKey

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
		NL -> as
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
		NL -> []
		ConsL x xs' -> x : matches xs'
