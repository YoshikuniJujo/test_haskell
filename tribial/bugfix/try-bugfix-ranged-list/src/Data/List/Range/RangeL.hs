{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
	UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=Plugin.TypeCheck.Nat.Simple #-}

module Data.List.Range.RangeL (
	LengthL,
	-- ** Type
	RangeL(..),
	-- ** PushL
	PushL, (.:..),
	-- ** AddL
	AddL, (++.),
	-- ** LoosenLMin and LoosenLMax
	-- *** loosenL
	loosenL,
	-- *** loosenLMin
	LoosenLMin, loosenLMin,
	-- *** loosenLMax
	LoosenLMax, loosenLMax,
	-- ** Unfoldr
	-- *** class
	Unfoldr,
	-- *** unfoldrRange
	-- **** without monad
	unfoldrRange, unfoldrRangeWithBase, unfoldrRangeWithBaseWithS,
	-- **** with monad
	unfoldrMRange, unfoldrMRangeWithBase,
	-- *** unfoldrRangeMaybe
	-- **** without monad
	unfoldrRangeMaybe, unfoldrRangeMaybeWithBase,
	-- **** with monad
	unfoldrMRangeMaybe, unfoldrMRangeMaybeWithBase,
	-- ** ZipL
	ZipL, zipL, zipWithL, zipWithML ) where

import GHC.TypeNats (Nat, type (+), type (-), type (<=))
import GHC.Exts (IsList(..))
import Control.Arrow (first, second, (***), (&&&))
import Control.Monad.Identity (Identity(..))
import Control.Monad.State (StateL(..))
import Data.Kind (Type)
import Data.Foldable (toList)
import Data.Bool (bool)
import Data.Maybe (isJust, fromMaybe)
import Data.String

type LengthL n = RangeL n n

{-^

The value of @LengthL n a@ is a list which have just @n@ members of type @a@.
You can push and pop an element from left.

>>> :set -XDataKinds
>>> sampleLengthL = 'h' :. 'e' :. 'l' :. 'l' :. 'o' :. NilL :: LengthL 5 Char

-}

---------------------------------------------------------------------------

-- * TYPE
--	+ RANGE LEFT
--	+ INSTANCE FUNCTOR
--	+ INSTANCE FOLDABLE
-- * PUSH
-- * ADD
-- * LOOSEN
-- 	+ LOOSEN LEFT
-- 	+ LOOSEN LEFT MIN
-- 	+ LOOSEN LEFT MAX
-- * UNFOLDR
-- 	+ CLASS
-- 	+ INSTANCE
-- 	+ UNFOLDR RANGE
-- 	+ UNFOLDR RANGE MAYBE
-- * ZIP
--	+ CLASS
--	+ FUNCTION

---------------------------------------------------------------------------
-- TYPE
---------------------------------------------------------------------------

-- RANGE LEFT

data RangeL :: Nat -> Nat -> Type -> Type where
	NilL :: 0 <= m => RangeL 0 m a
	(:..) :: 1 <= m => a -> RangeL 0 (m - 1) a -> RangeL 0 m a
	(:.) :: (1 <= n, 1 <= m) =>
		a -> RangeL (n - 1) (m - 1) a -> RangeL n m a

{-^

The value of @RangeL n m a@ is a list of type @a@ values whose element number is
at minimum @n@, and at maximum @m@.
You can push and pop an element from left.

>>> :set -XDataKinds
>>> sampleRangeL = 'h' :. 'e' :. 'l' :. 'l' :.. 'o' :.. NilL :: RangeL 3 8 Char

-}

infixr 6 :., :..

deriving instance Eq a => Eq (RangeL n m a)
deriving instance Ord a => Ord (RangeL n m a)
deriving instance Show a => Show (RangeL n m a)

-- INSTANCE FUNCTOR

instance Functor (RangeL 0 0) where _ `fmap` NilL = NilL

instance {-# OVERLAPPABLE #-}
	Functor (RangeL 0 (m - 1)) => Functor (RangeL 0 m) where
	fmap f = \case NilL -> NilL; x :.. xs -> f x :.. (f <$> xs)

instance {-# OVERLAPPABLE #-}
	(1 <= n, Functor (RangeL (n - 1) (m - 1))) => Functor (RangeL n m) where
	f `fmap` (x :. xs) = f x :. (f <$> xs)

-- INSTANCE FOLDABLE

instance Foldable (RangeL 0 0) where foldr _ z NilL = z

instance {-# OVERLAPPABLE #-}
	Foldable (RangeL 0 (m - 1)) => Foldable (RangeL 0 m) where
	foldr (-<) z = \case NilL -> z; x :.. xs -> x -< foldr (-<) z xs

instance {-# OVERLAPPABLE #-} (1 <= n, Foldable (RangeL (n - 1) (m - 1))) =>
	Foldable (RangeL n m) where
	foldr (-<) z (x :. xs) = x -< foldr (-<) z xs

-- INSTANCE TRAVERSABLE

instance Traversable (RangeL 0 0) where traverse _ NilL = pure NilL

instance {-# OVERLAPPABLE #-}
	Traversable (RangeL 0 (m - 1)) => Traversable (RangeL 0 m) where
	traverse f = \case
		NilL -> pure NilL; x :.. xs -> (:..) <$> f x <*> traverse f xs

instance {-# OVERLAPPABLE #-} (1 <= n, Traversable (RangeL (n - 1) (m - 1))) =>
	Traversable (RangeL n m) where
	traverse f (x :. xs) = (:.) <$> f x <*> traverse f xs

-- INSTANCE APPLICATIVE

instance Applicative (LengthL 0) where pure _ = NilL; _ <*> _ = NilL

instance {-# OVERLAPPABLE #-} (
	0 <= m,
	Functor (RangeL 0 m), Applicative (RangeL 0 (m - 1)), Unfoldr 0 0 m) =>
	Applicative (RangeL 0 m) where
	pure = unfoldrRange (const True) (\x -> (x, x))
	NilL <*> _ = NilL
	_ <*> NilL = NilL
	f :.. fs <*> x :.. xs = f x :.. (fs <*> xs)

instance {-# OVERLAPPABLE #-} (
	1 <= n, 0 <= m,
	Functor (RangeL n m), Applicative (RangeL (n - 1) (m - 1)),
	Unfoldr 0 n m) =>
	Applicative (RangeL n m) where
	pure = unfoldrRange (const True) (\x -> (x, x))
	f :. fs <*> x :. xs = f x :. (fs <*> xs)

instance Applicative (LengthL 0) => Monad (LengthL 0) where
	NilL >>= _ = NilL

instance {-# OVERLAPPABLE #-} (
	1 <= n, Applicative (LengthL n), Monad (LengthL (n - 1)) ) =>
	Monad (LengthL n) where
	x :. xs >>= f = y :. (xs >>= \z -> case f z of _ :. zs -> zs)
		where y :. _ = f x

-- INSTANCE ISSTRING

instance (0 <= m, Unfoldr 0 n m) => IsString (RangeL n m Char) where
	fromString s = fromMaybe (error $ "The string " ++ show s ++ " is not within range.")
		$ unfoldrRangeMaybe (\case "" -> Nothing; c : cs -> Just (c, cs)) s

instance (0 <= m, Foldable (RangeL n m), Unfoldr 0 n m) => IsList (RangeL n m a) where
	type Item (RangeL n m a) = a
	fromList lst = fromMaybe (error $ "The list is not within range.")
		$ unfoldrRangeMaybe (\case [] -> Nothing; x : xs -> Just (x, xs)) lst
	toList = Data.Foldable.toList

---------------------------------------------------------------------------
-- PUSH
---------------------------------------------------------------------------

infixr 5 .:..

class PushL n m where
	(.:..) :: a -> RangeL n (m - 1) a -> RangeL n m a

	{-^

	To push an optional element.

	>>> :set -XDataKinds
	>>> samplePushL = 'e' :. 'l' :. 'l' :. 'o' :.. NilL :: RangeL 3 7 Char
	>>> 'h' .:.. samplePushL
	'h' :. ('e' :. ('l' :. ('l' :.. ('o' :.. NilL))))
	>>> :type 'h' .:.. samplePushL
	'h' .:.. samplePushL :: RangeL 3 8 Char

	-}

instance 1 <= m => PushL 0 m where (.:..) = (:..)

instance {-# OVERLAPPABLE #-} (1 <= n, 1 <= m, PushL (n - 1) (m - 1)) => PushL n m where
	x .:.. y :. ys = x :. (y .:.. ys)

---------------------------------------------------------------------------
-- ADD
---------------------------------------------------------------------------

infixr 5 ++.

class AddL n m v w where
	(++.) :: RangeL n m a -> RangeL v w a -> RangeL (n + v) (m + w) a

	{-^

	To concatenate two lists
	whose types are @RangeL n m a@ and @RangeL v w a@.

	>>> :set -XDataKinds
	>>> sampleAddL1 = 'f' :. 'o' :. 'o' :.. NilL :: RangeL 2 5 Char
	>>> sampleAddL2 = 'b' :. 'a' :.. 'r' :.. NilL :: RangeL 1 6 Char
	>>> sampleAddL1 ++. sampleAddL2
	'f' :. ('o' :. ('o' :. ('b' :.. ('a' :.. ('r' :.. NilL)))))
	>>> :type sampleAddL1 ++. sampleAddL2
	sampleAddL1 ++. sampleAddL2 :: RangeL 3 11 Char

	-}

instance AddL 0 0 v w where NilL ++. ys = ys

instance {-# OVERLAPPABLE #-}
	(PushL v (m + w), AddL 0 (m - 1) v w, LoosenLMax v w (m + w)) =>
	AddL 0 m v w where
	(++.) :: forall a .  RangeL 0 m a -> RangeL v w a -> RangeL v (m + w) a
	NilL ++. ys = loosenLMax ys
	x :.. xs ++. ys = x .:.. (xs ++. ys :: RangeL v (m + w - 1) a)

instance {-# OVERLAPPABLE #-}
	(1 <= n, 1 <= n + v, 1 <= m + w, AddL (n - 1) (m - 1) v w) =>
	AddL n m v w where
	x :. xs ++. ys = x :. (xs ++. ys)

---------------------------------------------------------------------------
-- LOOSEN
---------------------------------------------------------------------------

-- LOOSEN LEFT

loosenL :: (LoosenLMin n m v, LoosenLMax v m w) => RangeL n m a -> RangeL v w a
loosenL = loosenLMax . loosenLMin

{-^

To loosen a range of an element number.

>>> :set -XDataKinds
>>> sampleLoosenL = 'h' :. 'e' :. 'l' :. 'l' :. 'o' :.. NilL :: RangeL 4 6 Char
>>> loosenL sampleLoosenL :: RangeL 2 8 Char
'h' :. ('e' :. ('l' :.. ('l' :.. ('o' :.. NilL))))

-}

-- LOOSEN LEFT MIN

class LoosenLMin n m v where
	loosenLMin :: RangeL n m a -> RangeL v m a

	{-^

	To loosen a lower bound of an element number.

	>>> :set -XDataKinds -fno-warn-tabs
	>>> :{
		sampleLoosenLMin :: RangeL 4 6 Char
		sampleLoosenLMin = 'h' :. 'e' :. 'l' :. 'l' :. 'o' :.. NilL
	:}

	>>> loosenLMin sampleLoosenLMin :: RangeL 2 6 Char
	'h' :. ('e' :. ('l' :.. ('l' :.. ('o' :.. NilL))))

	-}

instance LoosenLMin 0 m 0 where
	loosenLMin = \case NilL -> NilL; xs@(_ :.. _) -> xs

instance {-# OVERLAPPABLE #-}
	(1 <= n, LoosenLMin (n - 1) (m - 1) 0) => LoosenLMin n m 0 where
	loosenLMin (x :. xs) = x :.. loosenLMin xs
	
instance {-# OVERLAPPABLE #-}
	(1 <= n, 1 <= v, LoosenLMin (n - 1) (m - 1) (v - 1)) => LoosenLMin n m v where
	loosenLMin (x :. xs) = x :. loosenLMin xs

-- LOOSEN LEFT MAX

class LoosenLMax n m w where
	loosenLMax :: RangeL n m a -> RangeL n w a

	{-^

	To loosen an upper bound of an element number.

	>>> :set -XDataKinds -fno-warn-tabs
	>>> :{
		sampleLoosenLMax :: RangeL 4 6 Char
		sampleLoosenLMax = 'h' :. 'e' :. 'l' :. 'l' :. 'o' :.. NilL
	:}

	>>> loosenLMax sampleLoosenLMax :: RangeL 4 8 Char
	'h' :. ('e' :. ('l' :. ('l' :. ('o' :.. NilL))))

	-}

instance 0 <= w => LoosenLMax 0 0 w where loosenLMax NilL = NilL

instance {-# OVERLAPPABLE #-}
	(0 <= w, 1 <= w, LoosenLMax 0 (m - 1) (w - 1)) => LoosenLMax 0 m w where
	loosenLMax = \case NilL -> NilL; (x :.. xs) -> x :.. loosenLMax xs

instance {-# OVERLAPPABLE #-}
	(1 <= n, 1 <= w, LoosenLMax (n - 1) (m - 1) (w - 1)) => LoosenLMax n m w where
	loosenLMax (x :. xs) = x :. loosenLMax xs

---------------------------------------------------------------------------
-- UNFOLDR
---------------------------------------------------------------------------

-- CLASS

class Unfoldr n v w where
	unfoldrMRangeWithBase :: Monad m =>
		RangeL n w a -> m Bool -> m a -> m (RangeL v w a)

	{-^
	
	It is like @unfoldrMRange@. But it has already prepared values.

	>>> :set -XDataKinds
	>>> :module + Data.IORef
	>>> r <- newIORef 1
	>>> check = (< 3) <$> readIORef r
	>>> count = readIORef r >>= \n -> n * 3 <$ writeIORef r (n + 1)
	>>> xs = 123 :. 456 :.. NilL :: RangeL 1 5 Integer
	>>> unfoldrMRangeWithBase xs check count :: IO (RangeL 3 5 Integer)
	123 :. (456 :. (3 :. (6 :.. NilL)))
	
	-}

	unfoldrMRangeMaybeWithBase :: Monad m =>
		RangeL n w a -> m Bool -> m a -> m (Maybe (RangeL v w a))

	{-^
	
	It is like @unfoldrMRangeMaybe@.
	But it has already prepared values.

	>>> :set -XDataKinds -fno-warn-tabs
	>>> :module + Data.IORef
	>>> r <- newIORef 1
	>>> check = (< 3) <$> readIORef r
	>>> count = readIORef r >>= \n -> n * 3 <$ writeIORef r (n + 1)
	>>> xs = 123 :. 456 :.. NilL :: RangeL 1 5 Integer
	>>> :{
		unfoldrMRangeMaybeWithBase xs check count
			:: IO (Maybe (RangeL 3 5 Integer))
	:}
	Just (123 :. (456 :. (3 :. (6 :.. NilL))))
	
	-}

-- INSTANCE

instance Unfoldr 0 0 0 where
	unfoldrMRangeWithBase NilL _ _ = pure NilL
	unfoldrMRangeMaybeWithBase NilL p _ = bool (Just NilL) Nothing <$> p

instance {-# OVERLAPPABLE #-} (0 <= w - 1, 1 <= w, Unfoldr 0 0 (w - 1)) =>
	Unfoldr 0 0 w where
	unfoldrMRangeWithBase NilL p f =
		(p >>=) . bool (pure NilL) $ f >>= \x ->
			(x :..) <$> unfoldrMRangeWithBase NilL p f
	unfoldrMRangeWithBase (x :.. xs) p f =
		(x :..) <$> unfoldrMRangeWithBase xs p f

	unfoldrMRangeMaybeWithBase NilL p f =
		(p >>=) . bool (pure $ Just NilL) $ f >>= \x ->
			((x :..) <$>) <$> unfoldrMRangeMaybeWithBase NilL p f
	unfoldrMRangeMaybeWithBase (x :.. xs) p f =
		((x :..) <$>) <$> unfoldrMRangeMaybeWithBase xs p f

instance {-# OVERLAPPABLE #-}
	(1 <= v, 0 <= w - 1, 1 <= w, Unfoldr 0 (v - 1) (w - 1)) => Unfoldr 0 v w where
	unfoldrMRangeWithBase NilL p f =
		f >>= \x -> (x :.) <$> unfoldrMRangeWithBase NilL p f
	unfoldrMRangeWithBase (x :.. xs) p f =
		(x :.) <$> unfoldrMRangeWithBase xs p f

	unfoldrMRangeMaybeWithBase NilL p f =
		(p >>=) . bool (pure Nothing) $ f >>= \x ->
			((x :.) <$>) <$> unfoldrMRangeMaybeWithBase NilL p f
	unfoldrMRangeMaybeWithBase (x :.. xs) p f =
		((x :.) <$>) <$> unfoldrMRangeMaybeWithBase xs p f

instance {-# OVERLAPPABLE #-}
	(1 <= n, 1 <= v, Unfoldr (n - 1) (v - 1) (w - 1)) => Unfoldr n v w where
	unfoldrMRangeWithBase (x :. xs) p f =
		(x :.) <$> unfoldrMRangeWithBase xs p f

	unfoldrMRangeMaybeWithBase (x :. xs) p f =
		((x :.) <$>) <$> unfoldrMRangeMaybeWithBase xs p f

-- UNFOLDR RANGE

unfoldrRange :: (0 <= w, Unfoldr 0 v w) =>
	(s -> Bool) -> (s -> (a, s)) -> s -> RangeL v w a
unfoldrRange = unfoldrRangeWithBase NilL

{-^

To evaluate a function to construct a list.
The function recieve a state and return an element and a new state.
First argument is a predication which is evaluated
when an element number is greater than a minimum and not greater than a maximum.

>>> :set -XDataKinds
>>> next n = (n * 3, n + 1)
>>> unfoldrRange (< 2) next 1 :: RangeL 3 5 Int
3 :. (6 :. (9 :. NilL))

>>> unfoldrRange (< 5) next 1 :: RangeL 3 5 Int
3 :. (6 :. (9 :. (12 :.. NilL)))

>>> unfoldrRange (< 10) next 1 :: RangeL 3 5 Int
3 :. (6 :. (9 :. (12 :.. (15 :.. NilL))))

-}

unfoldrRangeWithBase :: Unfoldr n v w =>
	RangeL n w a -> (s -> Bool) -> (s -> (a, s)) -> s -> RangeL v w a
unfoldrRangeWithBase xs p f = fst . unfoldrRangeWithBaseWithS xs p f

{-^

It is like @unfoldrRange@. But it has already prepared values.

>>> :set -XDataKinds
>>> xs = 123 :. 456 :.. NilL :: RangeL 1 5 Integer
>>> unfoldrRangeWithBase xs (< 3) (\n -> (n * 3, n + 1)) 1 :: RangeL 3 5 Integer
123 :. (456 :. (3 :. (6 :.. NilL)))

-}

unfoldrRangeWithBaseWithS :: Unfoldr n v w =>
	RangeL n w a -> (s -> Bool) -> (s -> (a, s)) -> s -> (RangeL v w a, s)
unfoldrRangeWithBaseWithS xs p f =
	runStateL $ unfoldrMRangeWithBase xs (StateL $ p &&& id) (StateL f)

{-^

It is like @unfoldrRangeWithBase@.
But it return not only a list but also a state value.

>>> :set -XDataKinds
>>> next n = (n * 3, n + 1)
>>> xs = 123 :. 456 :.. NilL :: RangeL 1 5 Integer
>>> unfoldrRangeWithBaseWithS xs (< 3) next 1 :: (RangeL 3 5 Integer, Integer)
(123 :. (456 :. (3 :. (6 :.. NilL))),3)

-}

unfoldrMRange :: (0 <= w, Unfoldr 0 v w, Monad m) => m Bool -> m a -> m (RangeL v w a)
unfoldrMRange = unfoldrMRangeWithBase NilL

{-^

It is like @unfoldrRange@. But it use a monad instead of a function.

>>> :set -XDataKinds
>>> :module + Data.IORef
>>> r <- newIORef 1
>>> count = readIORef r >>= \n -> n * 3 <$ writeIORef r (n + 1)
>>> unfoldrMRange ((< 5) <$> readIORef r) count :: IO (RangeL 3 5 Integer)
3 :. (6 :. (9 :. (12 :.. NilL)))

-}

-- UNFOLDR RANGE MAYBE

unfoldrRangeMaybe :: (0 <= w, Unfoldr 0 v w) =>
	(s -> Maybe (a, s)) -> s -> Maybe (RangeL v w a)
unfoldrRangeMaybe = unfoldrRangeMaybeWithBase NilL

{-^

To eveluate a function to construct a list.
The function recieve a state and
return a nothing value or an element and a new state.
If number of created elements is less than a minimum number of list elements or
greater than a maximum number, then return Nothing.

>>> :set -XDataKinds
>>> next n0 n = if n < n0 then Just (n * 3, n + 1) else Nothing
>>> unfoldrRangeMaybe (next 2) 1 :: Maybe (RangeL 3 5 Int)
Nothing

>>> unfoldrRangeMaybe (next 5) 1 :: Maybe (RangeL 3 5 Int)
Just (3 :. (6 :. (9 :. (12 :.. NilL))))

>>> unfoldrRangeMaybe (next 10) 1 :: Maybe (RangeL 3 5 Int)
Nothing

-}

unfoldrRangeMaybeWithBase :: Unfoldr n v w =>
	RangeL n w a -> (s -> Maybe (a, s)) -> s -> Maybe (RangeL v w a)
unfoldrRangeMaybeWithBase xs f =
	fst . unfoldrRangeMaybeWithBaseGen xs (isJust &&& id)
		(maybe (error "never occur") (f `second`)) . f

{-^

It is like @unfoldrRangeMaybe@. But it has already prepared values.

>>> :set -XDataKinds
>>> xs = 123 :. 456 :.. NilL :: RangeL 1 5 Int
>>> next n = if n < 3 then Just (n * 3, n + 1) else Nothing
>>> unfoldrRangeMaybeWithBase xs next 1 :: Maybe (RangeL 3 5 Int)
Just (123 :. (456 :. (3 :. (6 :.. NilL))))

-}

type St a s r = Maybe (a, s) -> (r, Maybe (a, s))

unfoldrRangeMaybeWithBaseGen :: Unfoldr n v w =>
	RangeL n w a -> St a s Bool -> St a s a -> St a s (Maybe (RangeL v w a))
unfoldrRangeMaybeWithBaseGen xs p f =
	runStateL $ unfoldrMRangeMaybeWithBase xs (StateL p) (StateL f)

unfoldrMRangeMaybe :: (0 <= w, Unfoldr 0 v w, Monad m) =>
	m Bool -> m a -> m (Maybe (RangeL v w a))
unfoldrMRangeMaybe = unfoldrMRangeMaybeWithBase NilL

{-^

It is like @unfoldrRangeMaybe@.
But it use a monad instead of a function.
The first argument monad return boolean value.
It create values while this boolean value is True.
If this boolean value is False before to create enough values or
True after to create full values, then @unfoldrMRangeMaybe@ return Nothing.

>>> :set -XDataKinds
>>> :module + Data.IORef
>>> r <- newIORef 1
>>> check n0 = (< n0) <$> readIORef r
>>> count = readIORef r >>= \n -> n * 3 <$ writeIORef r (n + 1)
>>> unfoldrMRangeMaybe (check 2) count :: IO (Maybe (RangeL 3 5 Integer))
Nothing

>>> writeIORef r 1
>>> unfoldrMRangeMaybe (check 5) count :: IO (Maybe (RangeL 3 5 Integer))
Just (3 :. (6 :. (9 :. (12 :.. NilL))))

>>> writeIORef r 1
>>> unfoldrMRangeMaybe (check 10) count :: IO (Maybe (RangeL 3 5 Integer))
Nothing

-}

---------------------------------------------------------------------------
-- ZIP
---------------------------------------------------------------------------

-- CLASS

class ZipL n m v w where
	zipWithML :: Monad q =>
		(a -> b -> q c) -> RangeL n m a -> RangeL v w b ->
		q (RangeL n m c, RangeL (v - m) (w - n) b)

	{-^

	It is like @zipWithL@.
	But it use a function which return a monad instead of a simple value.

	>>> :set -XDataKinds -fno-warn-tabs
	>>> ns = 1 :. 2 :. 3 :.. NilL :: RangeL 2 4 Int
	>>> :{
		cs :: RangeL 5 7 Char
		cs = 'a' :. 'b' :. 'c' :. 'd' :. 'e' :. 'f' :.. NilL
	:}

	>>> zipWithML (\n -> putStrLn . replicate n) ns cs
	a
	bb
	ccc
	(() :. (() :. (() :.. NilL)),'d' :. ('e' :.. ('f' :.. NilL)))

	-}

instance ZipL 0 0 v w where zipWithML _ NilL = pure . (NilL ,)

instance {-# OVERLAPPABLE #-} (
	1 <= v, m <= v, LoosenLMin v w (v - m), LoosenLMax (v - m) (w - 1) w,
	ZipL 0 (m - 1) (v - 1) (w - 1) ) => ZipL 0 m v w where
	zipWithML _ NilL ys = pure (NilL, loosenLMin ys)
	zipWithML (%) (x :.. xs) (y :. ys) =
		x % y >>= \z -> ((z :..) *** loosenLMax) <$> zipWithML (%) xs ys

instance {-# OVERLAPPABLE #-} (
	1 <= n, 1 <= v, n <= w, m <= v,
	ZipL (n - 1) (m - 1) (v - 1) (w - 1) ) => ZipL n m v w where
	zipWithML (%) (x :. xs) (y :. ys) =
		x % y >>= \z -> ((z :.) `first`) <$> zipWithML (%) xs ys

-- FUNCTION

zipL :: ZipL n m v w => RangeL n m a -> RangeL v w b ->
	(RangeL n m (a, b), RangeL (v - m) (w - n) b)
zipL = zipWithL (,)

{-^

To recieve two lists and return a tuple list and rest of the second list.
The first list must be shorter or equal than the second list.

>>> :set -XDataKinds
>>> sampleZipL1 = 1 :. 2 :. 3 :.. NilL :: RangeL 2 4 Integer
>>> sampleZipL2 = 7 :. 6 :. 5 :. 4 :. 3 :. 2 :.. NilL :: RangeL 5 7 Integer
>>> zipL sampleZipL1 sampleZipL2
((1,7) :. ((2,6) :. ((3,5) :.. NilL)),4 :. (3 :.. (2 :.. NilL)))
>>> :type zipL sampleZipL1 sampleZipL2
zipL sampleZipL1 sampleZipL2
  :: (RangeL 2 4 (Integer, Integer), RangeL 1 5 Integer)

-}

zipWithL :: ZipL n m v w => (a -> b -> c) -> RangeL n m a -> RangeL v w b ->
	(RangeL n m c, RangeL (v - m) (w - n) b)
zipWithL op = (runIdentity .) . zipWithML ((Identity .) . op)

{-^

It is like @zipL@.
But it evaluate a function to make values instead of put together in tuples.

>>> :set -XDataKinds
>>> sampleZipWithL1 = 1 :. 2 :. 3 :.. NilL :: RangeL 2 4 Integer
>>> sampleZipWithL2 = 7 :. 6 :. 5 :. 4 :. 3 :. 2 :.. NilL :: RangeL 5 7 Integer
>>> zipWithL (+) sampleZipWithL1 sampleZipWithL2
(8 :. (8 :. (8 :.. NilL)),4 :. (3 :.. (2 :.. NilL)))
>>> :type zipWithL (+) sampleZipWithL1 sampleZipWithL2
zipWithL (+) sampleZipWithL1 sampleZipWithL2
  :: (RangeL 2 4 Integer, RangeL 1 5 Integer)

-}
