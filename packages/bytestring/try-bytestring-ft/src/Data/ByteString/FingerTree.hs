{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns, BangPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.ByteString.FingerTree (

	-- * BYTE STRING

	ByteString, pattern Empty, pattern (:<|), pattern (:|>),

	-- * INTRODUCING AND ELIMINATING

	empty, singleton, pack, unpack, fromStrict, toStrict,

	-- * BASIC INTERFACE

	cons, snoc, append, uncons, unsnoc, null, length,

	-- * GENERATING

	replicate,

	-- * BREAKING

	splitAt'

) where

import Prelude hiding (concat, null, length, replicate)
import Prelude qualified as P
import Data.Foldable (foldr')
import Data.String
import Data.Bool
import Data.Word
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC

import GHC.Generics

infixr 5 `consTree`
infixl 5 `snocTree`
infixr 5 `appendTree0`

newtype ByteString = ByteString { unByteString :: FingerTree BS.ByteString }

instance IsString ByteString where
	{-# INLINE fromString #-}
	fromString = packChars

instance Semigroup ByteString where
	(<>) = append

instance Monoid ByteString where
	mempty = Empty
	mappend = (<>)
	mconcat = concat

instance Read ByteString where
	readsPrec p str = [ (packChars x, y) | (x, y) <- readsPrec p str ]

instance Show ByteString where
	showsPrec p ps r = showsPrec p (unpackChars ps) r

instance Eq ByteString where (==) = eq
instance Ord ByteString where compare = compareBytes

empty :: ByteString
empty = Empty

singleton :: Word8 -> ByteString
singleton = ByteString . Single . BS.singleton

pack :: [Word8] -> ByteString
pack = \case
	[] -> ByteString EmptyT
	bs -> ByteString . Single $ BS.pack bs

unpack :: ByteString -> [Word8]
unpack (ByteString t) = unpackTree t

unpackTree :: FingerTree BS.ByteString -> [Word8]
unpackTree EmptyT = []
unpackTree (bs :<|| t) = BS.unpack bs ++ unpackTree t

fromStrict :: BS.ByteString -> ByteString
fromStrict = ByteString . Single

toStrict :: ByteString -> BS.ByteString
toStrict (ByteString t) = toStrictTree t

toStrictTree :: FingerTree BS.ByteString -> BS.ByteString
toStrictTree = BS.concat . toList

cons :: Word8 -> ByteString -> ByteString
b `cons` ByteString bs = ByteString $ BS.singleton b `consTree` bs

snoc :: ByteString -> Word8 -> ByteString
ByteString bs `snoc` b = ByteString $ bs `snocTree` BS.singleton b

append :: ByteString -> ByteString -> ByteString
append = (><)

uncons :: ByteString -> Maybe (Word8, ByteString)
uncons (ByteString t) = case t of
	EmptyT -> Nothing
	bs :<|| t' -> case BS.uncons bs of
		Nothing -> error "unnormalized ByteString"
		Just (b, bs') -> Just (b, ByteString $ bs' :<|| t')

unsnoc :: ByteString -> Maybe (ByteString, Word8)
unsnoc (ByteString t) = case t of
	EmptyT -> Nothing
	t' :||> bs -> case BS.unsnoc bs of
		Nothing -> error "unnormalized ByteString"
		Just (bs', b) -> Just (ByteString $ t' :||> bs', b)

null :: ByteString -> Bool
null = (<= 0) . size . unByteString

length :: ByteString -> Int
length = size . unByteString

concat :: [ByteString] -> ByteString
concat = foldr append Empty

replicate :: Int -> Word8 -> ByteString
replicate n = ByteString . Single . BS.replicate n

splitAt' :: Int -> ByteString -> Maybe (ByteString, ByteString)
splitAt' n (ByteString t) = case search (\l _ -> l > n) t of
	Position l x r -> let (xl, xr) = BS.splitAt (n - size l) x in
		Just (	ByteString $ bool (`snocTree` xl) id (BS.null xl) l,
			ByteString $ xr `consTree` r )
	_ -> Nothing

eq :: ByteString -> ByteString -> Bool
eq Empty Empty = True
eq Empty _ = False
eq _ Empty = False
eq aa@(a :<| as) ba@(b :<| bs)
	| length aa /= length ba = False
	| otherwise = a == b && as `eq` bs

compareBytes :: ByteString -> ByteString -> Ordering
compareBytes Empty Empty = EQ
compareBytes Empty _ = LT
compareBytes _ Empty = GT
compareBytes (a :<| as) (b :<| bs)
	| a < b = LT
	| a > b = GT
	| otherwise = compareBytes as bs

packChars :: [Char] -> ByteString
packChars cs
	| P.null cs = ByteString EmptyT
	| otherwise = ByteString . Single $ BSC.pack cs

unpackChars :: ByteString -> [Char]
unpackChars (ByteString t) = unpackCharsTree t

unpackCharsTree :: FingerTree BS.ByteString -> [Char]
unpackCharsTree EmptyT = []
unpackCharsTree (bs :<|| t) = BSC.unpack bs ++ unpackCharsTree t

pattern Empty :: ByteString
pattern Empty = ByteString EmptyT

{-# COMPLETE (:<|), Empty #-}

pattern (:<|) :: Word8 -> ByteString -> ByteString
pattern b :<| bs <- (uncons -> Just (b, bs)) where
	b :<| bs = b `cons` bs

{-# COMPLETE (:|>), Empty #-}

pattern (:|>) :: ByteString -> Word8 -> ByteString
pattern bs :|> b <- (unsnoc -> Just (bs, b)) where
	bs :|> b = bs `snoc` b

{-# COMPLETE (:<||), EmptyT #-}

pattern (:<||) :: Sized a => a -> FingerTree a -> FingerTree a
pattern x :<|| xs <- (viewl -> x :< xs) where 
	x :<|| xs = x `consTree` xs

{-# COMPLETE (:||>), EmptyT #-}

pattern (:||>) :: Sized a => FingerTree a -> a -> FingerTree a
pattern xs :||> x <- (viewr -> xs :> x) where
	xs :||> x = xs `snocTree` x

(><) :: ByteString -> ByteString -> ByteString
ByteString xs >< ByteString ys = ByteString $ xs `appendTree0` ys

data SearchResult a
	= Position !(FingerTree a) a !(FingerTree a)
	| OnLeft
	| OnRight
	| Nowhere
	deriving (Eq, Ord, Show, Generic)

search :: Sized a =>
	(Int -> Int -> Bool) -> FingerTree a -> SearchResult a
search p t
	| p_left && p_right = OnLeft
	| not p_left && p_right = case searchTree p 0 t 0 of
		Split l x r -> Position l x r
	| not p_left && not p_right = OnRight
	| otherwise = Nowhere
	where
	p_left = p 0 vt
	p_right = p vt 0
	vt = size t

data Split t a = Split !t a !t

searchTree :: Sized a =>
	(Int -> Int -> Bool) -> Int -> FingerTree a -> Int -> Split (FingerTree a) a
searchTree _ _ EmptyT _ = error "EmptyT"
searchTree _ _ (Single x) _ = Split EmptyT x EmptyT
searchTree p vl (Deep _ pr m sf) vr
	| p vlp vmsr = case searchDigit p vl pr vmsr of
		Split l x r -> Split (maybe EmptyT digitToTree l) x (deepL r m sf)
	| p vlpm vsr = case searchTree p vlp m vsr of
		Split ml xs mr -> case searchNode p (vlp + size ml) xs (size mr + vsr) of
			Split l x r -> Split (deepR pr ml l) x (deepL r mr sf)
	| otherwise = case searchDigit p vlpm sf vr of
		Split l x r -> Split (deepR pr m l) x (maybe EmptyT digitToTree r)
	where
	vlp = vl + size pr
	vlpm = vlp + vm
	vmsr = vm + vsr
	vsr = size sf + vr
	vm = size m

searchNode :: Sized a =>
	(Int -> Int -> Bool) -> Int -> Node a -> Int -> Split (Maybe (Digit a)) a
searchNode p vl (Node2 _ a b) vr
	| p va vb = Split Nothing a (Just (One b))
	| otherwise = Split (Just (One a)) b Nothing
	where
	va = vl + size a
	vb = size b + vr
searchNode p vl (Node3 _ a b c) vr
	| p va vbc = Split Nothing a (Just (Two b c))
	| p vab vc = Split (Just (One a)) b (Just (One c))
	| otherwise = Split (Just (Two a b)) c Nothing
	where
	va = vl + size a
	vab = va + size b
	vc = size c + vr
	vbc = size b + vc

searchDigit :: Sized a =>
	(Int -> Int -> Bool) -> Int -> Digit a -> Int -> Split (Maybe (Digit a)) a
searchDigit _ vl (One a) vr = vl `seq` vr `seq` Split Nothing a Nothing
searchDigit p vl (Two a b) vr
	| p va vb = Split Nothing a (Just (One b))
	| otherwise = Split (Just (One a)) b Nothing
	where
	va = vl + size a
	vb = size b + vr
searchDigit p vl (Three a b c) vr
	| p va vbc = Split Nothing a (Just (Two b c))
	| p vab vc = Split (Just (One a)) b (Just (One c))
	| otherwise = Split (Just (Two a b)) c Nothing
	where
	va = vl + size a
	vab = va + size b
	vbc = size b + vc
	vc = size c + vr

searchDigit p vl (Four a b c d) vr
	| p va vbcd = Split Nothing a (Just (Three b c d))
	| p vab vcd = Split (Just (One a)) b (Just (Two c d))
	| p vabc vd = Split (Just (Two a b)) c (Just (One d))
	| otherwise = Split (Just (Three a b c)) d Nothing
	where
	va = vl + size a
	vab = va + size b
	vabc = vab + size c
	vbcd = size b + vcd
	vcd = size c + vd
	vd = size d + vr

data FingerTree a
	= EmptyT
	| Single a
	| Deep {-# UNPACK #-} !Int !(Digit a) (FingerTree (Node a)) !(Digit a)
	deriving (Show, Eq, Ord)

toList :: Sized a => FingerTree a -> [a]
toList EmptyT = []
toList (x :<|| xs) = x : toList xs

instance Sized a => Sized (FingerTree a) where
	{-# SPECIALIZE instance Sized (FingerTree BS.ByteString) #-}
	{-# SPECIALIZE instance Sized (FingerTree (Node a)) #-}
	size EmptyT = 0
	size (Single x) = size x
	size (Deep v _ _ _) = v

{-# SPECIALIZE consTree :: BS.ByteString -> FingerTree BS.ByteString -> FingerTree BS.ByteString #-}
{-# SPECIALIZE consTree :: Node a -> FingerTree (Node a) -> FingerTree (Node a) #-}
consTree :: Sized a => a -> FingerTree a -> FingerTree a
consTree a EmptyT = Single a
consTree a (Single b) = deep (One a) EmptyT (One b)
consTree a (Deep s (Four b c d e) m sf) = m `seq`
	Deep (size a + s) (Two a b) (node3 c d e `consTree` m) sf
consTree a (Deep s (Three b c d) m sf) = Deep (size a + s) (Four a b c d) m sf
consTree a (Deep s (Two b c) m sf) = Deep (size a + s) (Three a b c) m sf
consTree a (Deep s (One b) m sf) = Deep (size a + s) (Two a b) m sf

{-# SPECIALIZE snocTree :: FingerTree BS.ByteString -> BS.ByteString -> FingerTree BS.ByteString #-}
{-# SPECIALIZE snocTree :: FingerTree (Node a) -> Node a -> FingerTree (Node a) #-}
snocTree :: Sized a => FingerTree a -> a -> FingerTree a
snocTree EmptyT a = Single a
snocTree (Single a) b = deep (One a) EmptyT (One b)
snocTree (Deep s pr m (Four a b c d)) e = m `seq`
	Deep (s + size e) pr (m `snocTree` node3 a b c) (Two d e)
snocTree (Deep s pr m (Three a b c)) d = Deep (s + size d) pr m (Four a b c d)
snocTree (Deep s pr m (Two a b)) c = Deep (s + size c) pr m (Three a b c)
snocTree (Deep s pr m (One a)) b = Deep (s + size b) pr m (Two a b)

data Digit a
	= One a
	| Two a a
	| Three a a a
	| Four a a a a
	deriving (Show, Eq, Ord)

foldDigit :: (b -> b -> b) -> (a -> b) -> Digit a -> b
foldDigit (<+>) f = \case
	One a -> f a
	Two a b -> f a <+> f b
	Three a b c -> f a <+> f b <+> f c
	Four a b c d -> f a <+> f b <+> f c <+> f d

instance Foldable Digit where
	foldMap = foldDigit mappend

	foldr f z = \case
		One a -> a `f` z
		Two a b -> a `f` (b `f` z)
		Three a b c -> a `f` (b `f` (c `f` z))
		Four a b c d -> a `f` (b `f` (c `f` (d `f` z)))
	{-# INLINE foldr #-}

	foldl f z = \case
		One a -> z `f` a
		Two a b -> (z `f` a) `f` b
		Three a b c -> ((z `f` a) `f` b) `f` c
		Four a b c d -> (((z `f` a) `f` b) `f` c) `f` d
	{-# INLINE foldl #-}

	foldr' f !z = \case
		One a -> f a z
		Two a b -> f a $! f b z
		Three a b c -> f a $! f b $! f c z
		Four a b c d -> f a $! f b $! f c $! f d z
	{-# INLINE foldr' #-}

	foldl' f !z = \case
		One a -> f z a
		Two a b -> (f $! f z a) b
		Three a b c -> (f $! (f $! f z a) b) c
		Four a b c d -> (f $! (f $! (f $! f z a) b) c) d
	{-# INLINE foldl' #-}

	foldr1 f = \case
		One a -> a
		Two a b -> a `f` b
		Three a b c -> a `f` (b `f` c)
		Four a b c d -> a `f` (b `f` (c `f` d))

	foldl1 f = \case
		One a -> a
		Two a b -> a `f` b
		Three a b c -> (a `f` b) `f` c
		Four a b c d -> ((a `f` b) `f` c) `f` d

instance Functor Digit where
	{-# INLINE fmap #-}
	fmap f = \case 
		One a -> One $ f a
		Two a b -> Two (f a) (f b)
		Three a b c -> Three (f a) (f b) (f c)
		Four a b c d -> Four (f a) (f b) (f c) (f d)

instance Sized a => Sized (Digit a) where
	{-# INLINE size #-}
	size = foldl1 (+) . fmap size

data Node a
	= Node2 {-# UNPACK #-} !Int a a
	| Node3 {-# UNPACK #-} !Int a a a
	deriving (Show, Eq, Ord)

foldNode :: (b -> b -> b) -> (a -> b) -> Node a -> b
foldNode (<+>) f (Node2 _ a b) = f a <+> f b
foldNode (<+>) f (Node3 _ a b c) = f a <+> f b <+> f c

instance Foldable Node where
	foldMap = foldNode mappend

	foldr f z = \case
		Node2 _ a b -> a `f` (b `f` z)
		Node3 _ a b c -> a `f` (b `f` (c `f` z))
	{-# INLINE foldr #-}

	foldl f z = \case
		Node2 _ a b -> (z `f` a) `f` b
		Node3 _ a b c -> ((z `f` a) `f` b) `f` c
	{-# INLINE foldl #-}

	foldr' f !z = \case
		Node2 _ a b -> f a $! f b z
		Node3 _ a b c -> f a $! f b $! f c z
	{-# INLINE foldr' #-}

	foldl' f !z = \case
		Node2 _ a b -> (f $! f z a) b
		Node3 _ a b c -> (f $! (f $! f z a) b) c

instance Functor Node where
	{-# INLINE fmap #-}
	fmap f = \case
		Node2 v a b -> Node2 v (f a) (f b)
		Node3 v a b c -> Node3 v (f a) (f b) (f c)

instance Sized (Node a) where
	size = \case Node2 v _ _ -> v; Node3 v _ _ _ -> v

{-# INLINE node2 #-}
node2 :: Sized a => a -> a -> Node a
node2 a b = Node2 (size a + size b) a b

{-# INLINE node3 #-}
node3 :: Sized a => a -> a -> a -> Node a
node3 a b c = Node3 (size a + size b + size c) a b c

{-# INLINE deep #-}

deep :: Sized a => Digit a -> FingerTree (Node a) -> Digit a -> FingerTree a
deep pr m sf = Deep (size pr + size m + size sf) pr m sf

deepL :: Sized a =>
	Maybe (Digit a) -> FingerTree (Node a) -> Digit a -> FingerTree a
deepL Nothing m sf = rotL m sf
deepL (Just pr) m sf = deep pr m sf

rotL :: Sized a => FingerTree (Node a) -> Digit a -> FingerTree a
rotL m sf = case viewl m of
	EmptyL -> digitToTree sf
	a :< m' -> Deep (size m + size sf) (nodeToDigit a) m' sf

data ViewL s a = EmptyL | a :< s a deriving (Eq, Ord, Show, Read, Generic)

viewl :: Sized a => FingerTree a -> ViewL FingerTree a
viewl EmptyT = EmptyL
viewl (Single x) = x :< EmptyT
viewl (Deep _ (One x) m sf) = x :< rotL m sf
viewl (Deep _ pr m sf) = lheadDigit pr :< deep (ltailDigit pr) m sf

lheadDigit :: Digit a -> a
lheadDigit (One a) = a
lheadDigit (Two a _) = a
lheadDigit (Three a _ _) = a
lheadDigit (Four a _ _ _) = a

ltailDigit :: Digit a -> Digit a
ltailDigit (One _) = error "bad"
ltailDigit (Two _ b) = One b
ltailDigit (Three _ b c) = Two b c
ltailDigit (Four _ b c d) = Three b c d

deepR :: Sized a =>
	Digit a -> FingerTree (Node a) -> Maybe (Digit a) -> FingerTree a
deepR pr m Nothing = rotR pr m
deepR pr m (Just sf) = deep pr m sf

rotR :: Sized a => Digit a -> FingerTree (Node a) -> FingerTree a
rotR pr m = case viewr m of
	EmptyR -> digitToTree pr
	m' :> a -> Deep (size pr + size m) pr m' (nodeToDigit a)

data ViewR s a = EmptyR | s a :> a deriving (Eq, Ord, Show, Read, Generic)

viewr :: Sized a => FingerTree a -> ViewR FingerTree a
viewr EmptyT = EmptyR
viewr (Single x) = EmptyT :> x
viewr (Deep _ pr m (One x)) = rotR pr m :> x
viewr (Deep _ pr m sf) = deep pr m (rtailDigit sf) :> rheadDigit sf

rheadDigit :: Digit a -> a
rheadDigit (One a) = a
rheadDigit (Two _ b) = b
rheadDigit (Three _ _ c) = c
rheadDigit (Four _ _ _ d) = d

rtailDigit :: Digit a -> Digit a
rtailDigit (One _) = error "bad"
rtailDigit (Two a _) = One a
rtailDigit (Three a b _) = Two a b
rtailDigit (Four a b c _) = Three a b c

nodeToDigit :: Node a -> Digit a
nodeToDigit (Node2 _ a b) = Two a b
nodeToDigit (Node3 _ a b c) = Three a b c

digitToTree :: Sized a => Digit a -> FingerTree a
digitToTree (One a) = Single a
digitToTree (Two a b) = deep (One a) EmptyT (One b)
digitToTree (Three a b c) = deep (Two a b) EmptyT (One c)
digitToTree (Four a b c d) = deep (Two a b) EmptyT (Two c d)

class Sized a where size :: a -> Int

instance Sized BS.ByteString where size = BS.length

-- The appendTree/addDigits gunk below was originally machine generated via mkappend.hs
-- but has since been manually edited to include strictness annotations.

appendTree0 :: FingerTree (BS.ByteString) -> FingerTree (BS.ByteString) -> FingerTree (BS.ByteString)
appendTree0 EmptyT xs =
     xs
appendTree0 xs EmptyT =
     xs
appendTree0 (Single x) xs =
     x `consTree` xs
appendTree0 xs (Single x) =
     xs `snocTree` x
appendTree0 (Deep s1 pr1 m1 sf1) (Deep s2 pr2 m2 sf2) =
     Deep (s1 + s2) pr1 m sf2
     where !m = addDigits0 m1 sf1 pr2 m2

addDigits0 :: FingerTree (Node (BS.ByteString)) -> Digit (BS.ByteString) -> Digit (BS.ByteString) -> FingerTree (Node (BS.ByteString)) -> FingerTree (Node (BS.ByteString))
addDigits0 m1 (One a) (One b) m2 =
    appendTree1 m1 (node2 a b) m2
addDigits0 m1 (One a) (Two b c) m2 =
    appendTree1 m1 (node3 a b c) m2
addDigits0 m1 (One a) (Three b c d) m2 =
    appendTree2 m1 (node2 a b) (node2 c d) m2
addDigits0 m1 (One a) (Four b c d e) m2 =
    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits0 m1 (Two a b) (One c) m2 =
    appendTree1 m1 (node3 a b c) m2
addDigits0 m1 (Two a b) (Two c d) m2 =
    appendTree2 m1 (node2 a b) (node2 c d) m2
addDigits0 m1 (Two a b) (Three c d e) m2 =
    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits0 m1 (Two a b) (Four c d e f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits0 m1 (Three a b c) (One d) m2 =
    appendTree2 m1 (node2 a b) (node2 c d) m2
addDigits0 m1 (Three a b c) (Two d e) m2 =
    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits0 m1 (Three a b c) (Three d e f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits0 m1 (Three a b c) (Four d e f g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits0 m1 (Four a b c d) (One e) m2 =
    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits0 m1 (Four a b c d) (Two e f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits0 m1 (Four a b c d) (Three e f g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits0 m1 (Four a b c d) (Four e f g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2

appendTree1 :: FingerTree (Node a) -> Node a -> FingerTree (Node a) -> FingerTree (Node a)
appendTree1 EmptyT !a xs =
     a `consTree` xs
appendTree1 xs !a EmptyT =
     xs `snocTree` a
appendTree1 (Single x) !a xs =
     x `consTree` a `consTree` xs
appendTree1 xs !a (Single x) =
     xs `snocTree` a `snocTree` x
appendTree1 (Deep s1 pr1 m1 sf1) a (Deep s2 pr2 m2 sf2) =
     Deep (s1 + size a + s2) pr1 m sf2
     where !m = addDigits1 m1 sf1 a pr2 m2

addDigits1 :: FingerTree (Node (Node a)) -> Digit (Node a) -> Node a -> Digit (Node a) -> FingerTree (Node (Node a)) -> FingerTree (Node (Node a))
addDigits1 m1 (One a) b (One c) m2 =
    appendTree1 m1 (node3 a b c) m2
addDigits1 m1 (One a) b (Two c d) m2 =
    appendTree2 m1 (node2 a b) (node2 c d) m2
addDigits1 m1 (One a) b (Three c d e) m2 =
    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits1 m1 (One a) b (Four c d e f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits1 m1 (Two a b) c (One d) m2 =
    appendTree2 m1 (node2 a b) (node2 c d) m2
addDigits1 m1 (Two a b) c (Two d e) m2 =
    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits1 m1 (Two a b) c (Three d e f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits1 m1 (Two a b) c (Four d e f g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits1 m1 (Three a b c) d (One e) m2 =
    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits1 m1 (Three a b c) d (Two e f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits1 m1 (Three a b c) d (Three e f g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits1 m1 (Three a b c) d (Four e f g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits1 m1 (Four a b c d) e (One f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits1 m1 (Four a b c d) e (Two f g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits1 m1 (Four a b c d) e (Three f g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits1 m1 (Four a b c d) e (Four f g h i) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2

appendTree2 :: FingerTree (Node a) -> Node a -> Node a -> FingerTree (Node a) -> FingerTree (Node a)
appendTree2 EmptyT !a !b xs =
     a `consTree` b `consTree` xs
appendTree2 xs !a !b EmptyT =
     xs `snocTree` a `snocTree` b
appendTree2 (Single x) a b xs =
     x `consTree` a `consTree` b `consTree` xs
appendTree2 xs a b (Single x) =
     xs `snocTree` a `snocTree` b `snocTree` x
appendTree2 (Deep s1 pr1 m1 sf1) a b (Deep s2 pr2 m2 sf2) =
     Deep (s1 + size a + size b + s2) pr1 m sf2
     where !m = addDigits2 m1 sf1 a b pr2 m2

addDigits2 :: FingerTree (Node (Node a)) -> Digit (Node a) -> Node a -> Node a -> Digit (Node a) -> FingerTree (Node (Node a)) -> FingerTree (Node (Node a))
addDigits2 m1 (One a) b c (One d) m2 =
    appendTree2 m1 (node2 a b) (node2 c d) m2
addDigits2 m1 (One a) b c (Two d e) m2 =
    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits2 m1 (One a) b c (Three d e f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits2 m1 (One a) b c (Four d e f g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits2 m1 (Two a b) c d (One e) m2 =
    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits2 m1 (Two a b) c d (Two e f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits2 m1 (Two a b) c d (Three e f g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits2 m1 (Two a b) c d (Four e f g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits2 m1 (Three a b c) d e (One f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits2 m1 (Three a b c) d e (Two f g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits2 m1 (Three a b c) d e (Three f g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits2 m1 (Three a b c) d e (Four f g h i) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits2 m1 (Four a b c d) e f (One g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits2 m1 (Four a b c d) e f (Two g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits2 m1 (Four a b c d) e f (Three g h i) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits2 m1 (Four a b c d) e f (Four g h i j) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2

appendTree3 :: FingerTree (Node a) -> Node a -> Node a -> Node a -> FingerTree (Node a) -> FingerTree (Node a)
appendTree3 EmptyT !a !b !c xs =
     a `consTree` b `consTree` c `consTree` xs
appendTree3 xs !a !b !c EmptyT =
     xs `snocTree` a `snocTree` b `snocTree` c
appendTree3 (Single x) a b c xs =
     x `consTree` a `consTree` b `consTree` c `consTree` xs
appendTree3 xs a b c (Single x) =
     xs `snocTree` a `snocTree` b `snocTree` c `snocTree` x
appendTree3 (Deep s1 pr1 m1 sf1) a b c (Deep s2 pr2 m2 sf2) =
     Deep (s1 + size a + size b + size c + s2) pr1 m sf2
     where !m = addDigits3 m1 sf1 a b c pr2 m2

addDigits3 :: FingerTree (Node (Node a)) -> Digit (Node a) -> Node a -> Node a -> Node a -> Digit (Node a) -> FingerTree (Node (Node a)) -> FingerTree (Node (Node a))
addDigits3 m1 (One a) b c d (One e) m2 =
    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits3 m1 (One a) b c d (Two e f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits3 m1 (One a) b c d (Three e f g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits3 m1 (One a) b c d (Four e f g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits3 m1 (Two a b) c d e (One f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits3 m1 (Two a b) c d e (Two f g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits3 m1 (Two a b) c d e (Three f g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits3 m1 (Two a b) c d e (Four f g h i) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits3 m1 (Three a b c) d e f (One g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits3 m1 (Three a b c) d e f (Two g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits3 m1 (Three a b c) d e f (Three g h i) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits3 m1 (Three a b c) d e f (Four g h i j) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
addDigits3 m1 (Four a b c d) e f g (One h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits3 m1 (Four a b c d) e f g (Two h i) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits3 m1 (Four a b c d) e f g (Three h i j) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
addDigits3 m1 (Four a b c d) e f g (Four h i j k) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node2 j k) m2

appendTree4 :: FingerTree (Node a) -> Node a -> Node a -> Node a -> Node a -> FingerTree (Node a) -> FingerTree (Node a)
appendTree4 EmptyT !a !b !c !d xs =
     a `consTree` b `consTree` c `consTree` d `consTree` xs
appendTree4 xs !a !b !c !d EmptyT =
     xs `snocTree` a `snocTree` b `snocTree` c `snocTree` d
appendTree4 (Single x) a b c d xs =
     x `consTree` a `consTree` b `consTree` c `consTree` d `consTree` xs
appendTree4 xs a b c d (Single x) =
     xs `snocTree` a `snocTree` b `snocTree` c `snocTree` d `snocTree` x
appendTree4 (Deep s1 pr1 m1 sf1) a b c d (Deep s2 pr2 m2 sf2) =
     Deep (s1 + size a + size b + size c + size d + s2) pr1 m sf2
     where !m = addDigits4 m1 sf1 a b c d pr2 m2

addDigits4 :: FingerTree (Node (Node a)) -> Digit (Node a) -> Node a -> Node a -> Node a -> Node a -> Digit (Node a) -> FingerTree (Node (Node a)) -> FingerTree (Node (Node a))
addDigits4 m1 (One a) b c d e (One f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits4 m1 (One a) b c d e (Two f g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits4 m1 (One a) b c d e (Three f g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits4 m1 (One a) b c d e (Four f g h i) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits4 m1 (Two a b) c d e f (One g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits4 m1 (Two a b) c d e f (Two g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits4 m1 (Two a b) c d e f (Three g h i) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits4 m1 (Two a b) c d e f (Four g h i j) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
addDigits4 m1 (Three a b c) d e f g (One h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits4 m1 (Three a b c) d e f g (Two h i) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits4 m1 (Three a b c) d e f g (Three h i j) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
addDigits4 m1 (Three a b c) d e f g (Four h i j k) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node2 j k) m2
addDigits4 m1 (Four a b c d) e f g h (One i) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits4 m1 (Four a b c d) e f g h (Two i j) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
addDigits4 m1 (Four a b c d) e f g h (Three i j k) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node2 j k) m2
addDigits4 m1 (Four a b c d) e f g h (Four i j k l) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node3 j k l) m2
