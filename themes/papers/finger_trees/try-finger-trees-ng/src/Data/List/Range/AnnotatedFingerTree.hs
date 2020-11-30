{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
	UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=New.TypeCheck.Nat #-}

module Data.List.Range.AnnotatedFingerTree where

import GHC.TypeNats

import Data.List.Range
import Data.View
import Internal.Tools

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

instance (Foldable (RangeL n m), Measured a v) => Measured (RangeL n m a) v where
	measure xs = reducel (\i a -> i <> measure a) mempty xs

instance (Foldable (RangeR n m), Measured a v) => Measured (RangeR n m a) v where
	measure xs = reducer (\a i -> measure a <> i) xs mempty

data FingerTree v a
	= Empty | Single a
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

viewL  :: Measured a v => FingerTree v a -> ViewL (FingerTree v) a
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

isEmpty :: Measured a v => FingerTree v a -> Bool
isEmpty x = case viewL x of NL -> True; ConsL _ _ -> False

uncons :: Measured a v => FingerTree v a -> Maybe (a, FingerTree v a)
uncons x = case viewL x of NL -> Nothing; ConsL a x' -> Just (a, x')

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

unsnoc :: Measured a v => FingerTree v a -> Maybe (FingerTree v a, a)
unsnoc x = case viewR x of NR -> Nothing; ConsR x' a -> Just (x', a)

app3 :: forall v a . Measured a v => FingerTree v a -> RangeL 0 4 a -> FingerTree v a -> FingerTree v a
app3 Empty ts xs = ts <|. xs
app3 xs ts Empty = xs |>. ts
app3 (Single x) ts xs = x <| ts <|. xs
app3 xs ts (Single x) = xs |>. ts |> x
app3 (Deep _ pr1 m1 sf1) ts (Deep _ pr2 m2 sf2) =
	deep pr1 (app3 m1 (loosenL (nodes (rightToLeft sf1 ++. ts ++. pr2) :: RangeL 1 4 (Node v a))) m2) sf2

class Nodes m m' where
	nodes :: Measured a v => RangeL 2 m a -> RangeL 1 m' (Node v a)

instance Nodes 3 1 where
	nodes (a :. b :. NilL) = node2 a b :. NilL
	nodes (a :. b :. c :.. NilL) = node3 a b c :. NilL
	nodes _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	(1 <= m' - 1, Nodes (m - 3) (m' - 1)) => Nodes m m' where
	nodes :: forall a v . Measured a v => RangeL 2 m a -> RangeL 1 m' (Node v a)
	nodes (a :. b :. NilL) = node2 a b :. NilL
	nodes (a :. b :. c :.. NilL) = node3 a b c :. NilL
	nodes (a :. b :. c :.. d :.. NilL) = node2 a b :. node2 c d :.. NilL
	nodes (a :. b :. c :.. d :.. e :.. xs) = node3 a b c .:.. (nodes (d :. e :. xs :: RangeL 2 (m - 3) a) :: RangeL 1 (m' - 1) (Node v a))
	nodes _ = error "never occur"

(><) :: Measured a v => FingerTree v a -> FingerTree v a -> FingerTree v a
xs >< ys = app3 xs NilL ys

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
		Split (deepR pr ml $ leftToRight l) x (deepL r mr sf)
	| otherwise = let Split l x r = splitDigitR p vm sf in
		Split (deepR pr m l) x (toTree r)
	where
	vpr = i <> measure pr
	vm = vpr <> measure m

split :: Measured a v =>
	(v -> Bool) -> FingerTree v a -> (FingerTree v a, FingerTree v a)
split p xs
	| p $ measure xs = (l, x <| r)
	| otherwise = (xs, Empty)
	where Split l x r = splitTree p mempty xs

takeUntil, dropUntil :: Measured a v => (v -> Bool) -> FingerTree v a -> FingerTree v a
takeUntil p = fst . split p
dropUntil p = snd . split p

newtype Elem a = Elem { getElem :: a } deriving Show
