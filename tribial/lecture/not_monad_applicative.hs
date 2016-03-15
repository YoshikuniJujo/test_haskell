{-# LANGUAGE TupleSections, MonadComprehensions #-}

-- m a -> m b -> m (a, b)
--
-- [1, 2, 3], [4, 5]
-- => [(1, 4), (1, 5), (2, 4), (2, 5), (3, 4), (3, 5)]

-- m (m a) -> m a
--
-- [[1, 2, 3], [4, 5], [6, 7]]
-- => [1, 2, 3, 4, 5, 6, 7]

import Control.Applicative
import Control.Arrow
import Data.Monoid
import Data.List

data Point a = Point Int a deriving Show

pointInc :: Point a -> Point a
pointInc (Point p x) = Point (p + 1) x

instance Functor Point where
	fmap f (Point p x) = Point p (f x)

instance Applicative Point where
	pure = Point 0
	Point p1 f <*> Point p2 x = Point (p1 + p2) $ f x

instance Monad Point where
	return = pure
	Point p1 x >>= f = let Point p2 y = f x in Point (p1 + p2) y

-- m a -> m b -> m (a, b)
-- Point 3 "hello", Point 5 "world"
-- => Point 8 ("hello", "world")

-- m (m a)
-- Point 3 (Point 5 "hello")
-- => Point 8 "hello"

-- a -> m b
-- [a] -> m b
--
-- ([a] -> m b) -> ([b] -> m c) -> ([a] -> m c)
--
-- data Parse b c = Parse [b] [c]

data Parse b c = Parse { getParse :: [(Maybe b, c)] } deriving Show

instance Functor (Parse b) where
	fmap f = Parse . map (second f) . getParse

instance Applicative (Parse b) where
	pure = Parse . (: []) . (Nothing ,)
--	Parse 
--	Parse Nothing f <*> Parse _ x = 

data Fun a b = Fun { getFun :: [(a -> Bool, b)] }

apply :: Fun a b -> a -> Maybe b
apply (Fun ps) x = case find (($ x) . fst) ps  of
	Just (_, y) -> Just y
	_ -> Nothing

instance Functor (Fun a) where
	fmap f = Fun . map (second f) . getFun

tup :: Applicative f => f a -> f b -> f (a, b)
x `tup` y = (,) <$> x <*> y

instance Applicative (Fun a) where
	pure = Fun . (: []) . (const True ,)
	Fun fs <*> Fun xs = Fun
		[ ((&&) <$> pf <*> px, f x) | (pf, f) <- fs, (px, x) <- xs ]

instance Monad (Fun a) where
	return = pure
--	xs >>= f = fjoin $ fmap f xs

-- fjoin :: Fun a (Fun a b) -> Fun a b
-- fjoin

data X = A | B | C deriving (Eq, Show)
data Y = D | E | F deriving (Eq, Show)

f1, f2 :: Fun X Y
f1 = Fun [((== A), D), ((== B), E)]
f2 = Fun [((== B), D), ((== C), E)]

-- m (m a) -> m a
-- Fun a (Fun a b) -> Fun a b
-- [(a -> Bool, Fun a b)]
-- [(a -> Bool, [(a -> Bool, b)])]

-- m a -> m b -> m (a, b)
-- m (m a) -> m a
--
-- mul     : OK
-- flattern: NG

data Two a = Two a a deriving Show

instance Functor Two where
	fmap f (Two x y) = Two (f x) (f y)

instance Applicative Two where
	pure x = Two x x
--	Two f g <*> Two x y = Two (f x) (g y)
	fs <*> xs = do { f <- fs; x <- xs; return $ f x }

instance Monad Two where
	return = pure
	Two x y >>= f = let
		Two x' _ = f x
		Two _ y' = f y in
		Two x' y'

-- Two (Two Int)
-- Two (Two 3 5) (Two 8 12)

data Flip a = Flip a a deriving Show

instance Functor Flip where
	fmap f (Flip x y) = Flip (f x) (f y)

instance Applicative Flip where
	pure x = Flip x x
--	Flip f g <*> Flip x y = Flip (f y) (g x)
	fs <*> xs = do { f <- fs; x <- xs; return $ f x }

instance Monad Flip where
	return = pure
	Flip x y >>= f = let
		Flip _ x' = f x
		Flip y' _ = f y in
		Flip x' y'

-- m a -> m b -> m (a, b)
-- m (m a) -> m a

data Three a = Three a a a deriving Show

instance Functor Three where
	fmap f (Three x y z) = Three (f x) (f y) (f z)

instance Applicative Three where
	pure x = Three x x x
--	fs <*> xs = do { f <- fs; x <- xs; return $ f x }
	fs <*> xs = [ f x | f <- fs, x <- xs ]

instance Monad Three where
	return = pure
	Three x y z >>= f = let
		Three x' _ _ = f x
		Three _ y' _ = f y
		Three _ _ z' = f z in
		Three x' y' z'

-- Monoid a => (a, b) -> (a, c) -> (a, (b, c))
-- Monoid a => (a, (a, b)) -> (a, b)

instance Monoid a => Monad ((,) a) where
	return = pure
	(a, x) >>= f = let (b, y) = f x in (a `mappend` b, y)

-- tup :: m a -> m b -> m (a, b)
-- join :: m (m a) -> m a

-- u :: m a, v :: m b
-- => join $ fmap (\y -> fmap (, y) u) v

-- [ (x, y) | x <- u, y <- v ]

-- u >>= \x -> v >>= \y -> return (x, y)
-- (>>=) u $ \x -> (>>=) v $ \y -> return (x, y)
-- (=<<) (\x -> fmap (\y -> (x, y)) v) u
-- join $ fmap (\x -> fmap (x ,) v) u

-- u `tup` v = join $ fmap (\x -> fmap (x ,) v) u

-- fmap :: (a -> b) -> f a -> f b
-- pamf :: f a -> (a -> b) -> f b
-- some :: m a -> m b -> m (m (a, b))
-- some u v = fmap (\x -> fmap (x ,) v) u
-- some u v = u `pamf` \x -> v `pamf` (x ,)

-- put :: m (a, b) -> (m a, m b)

data Foo a = Foo [a] | Bar Int | Unit a deriving Show

instance Functor Foo where
	fmap f (Foo x) = Foo $ map f x
	fmap _ (Bar n) = Bar n

instance Applicative Foo where
	pure x = fmap (const x) unit
	u <*> v = fmap (uncurry ($)) $ u .**. v
{-
	pure = Unit
	Foo fs <*> Foo xs = Foo $ fs <*> xs
	Foo fs <*> Bar n = Bar $ length fs `min` n
	Bar n <*> Foo xs = Bar $ n `min` length xs
	Bar n <*> Bar n' = Bar $ n `min` n'
	Unit f <*> Foo xs = Foo $ map f xs
	Unit _ <*> Bar n = Bar n
	Foo fs <*> Unit x = Foo $ map ($ x) fs
	Bar n <*> Unit _ = Bar n
	Unit f <*> Unit x = Unit $ f x
	-}

unit :: Applicative f => f ()
unit = pure ()

(.**) :: Applicative f => f a -> f b -> f (a, b)
u .** v = (,) <$> u <*> v

funit :: Foo ()
funit = Unit ()

(.**.) :: Foo a -> Foo b -> Foo (a, b)
Foo xs .**. Foo ys = Foo $ xs .** ys
Foo xs .**. Bar n = Bar $ length xs `min` n
Foo xs .**. Unit y = Foo $ map (, y) xs
Bar n .**. Foo ys = Bar $ n `min` length ys
Bar n .**. Bar n' = Bar $ n `min` n'
Bar n .**. Unit y = Bar n
Unit x .**. Foo ys = Foo $ map (x ,) ys
Unit x .**. Bar n = Bar n
Unit x .**. Unit y = Unit (x, y)
