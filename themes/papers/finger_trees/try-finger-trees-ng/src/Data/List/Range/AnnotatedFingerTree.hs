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

newtype Size = Size { getSize :: Int } deriving (Show, Eq, Ord)

instance Semigroup Size where Size m <> Size n = Size $ m + n
instance Monoid Size where mempty = Size 0

newtype Elem a = Elem { getElem :: a } deriving Show

instance Measured (Elem a) Size where measure _ = Size 1

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

sampleAnn1, sampleAnn2 :: FingerTree Size (Elem Int)
sampleAnn1 = toTree $ Elem <$> [1 .. 100]
sampleAnn2 = toTree $ Elem <$> [123 .. 200]
