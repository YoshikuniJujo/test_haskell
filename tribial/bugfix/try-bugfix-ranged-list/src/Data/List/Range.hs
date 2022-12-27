{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
	UndecidableInstances #-}
{-# OPTIOnS_GHC -Wall -fno-warn-tabs -fplugin=Plugin.TypeCheck.Nat.Simple #-}

module Data.List.Range (
	-- * RANGED LIST LEFT
	module Data.List.Range.RangeL,
	-- ** Repeat and Unfoldr Min and Max
	-- *** repeat
	repeatLMin, repeatLMax,
	-- *** unfoldr
	unfoldrMin, unfoldrMax,
	-- *** unfoldrM
	unfoldrMMin, unfoldrMMax,
	-- * RANGED LIST RIGHT
	module Data.List.Range.RangeR,
	-- ** Repeat and Unfoldl Min and Max
	-- *** repeat
	repeatRMin, repeatRMax,
	-- *** unfoldl
	unfoldlMin, unfoldlMax,
	-- *** unfoldlM
	unfoldlMMin, unfoldlMMax,
	-- * LEFT TO RIGHT
	LeftToRight, (++.+), leftToRight,
	-- * RIGHT TO LEFT
	RightToLeft, (++..), rightToLeft ) where

import GHC.TypeNats (type (+), type (-), type (<=))
import Data.List.Length.LengthL (unfoldr, unfoldrM)
import Data.List.Length.LengthR (unfoldl, unfoldlM)
import Data.List.Range.RangeL
import Data.List.Range.RangeR

---------------------------------------------------------------------------

-- * RANGED LIST LEFT
--	+ MIN
--	+ MAX
-- * RANGED LIST RIGHT
--	+ MIN
--	+ MAX
-- * LEFT TO RIGHT
--	+ CLASS
--	+ INSTANCE
--	+ FUNCTION
-- * RIGHT TO LEFT
--	+ CLASS
--	+ INSTANCE
--	+ FUNCTION

---------------------------------------------------------------------------
-- RANGED LIST LEFT
---------------------------------------------------------------------------

-- MIN

repeatLMin :: (0 <= n, LoosenLMax n n m, Unfoldr 0 n n) => a -> RangeL n m a
repeatLMin = unfoldrMin \x -> (x, x)

{-^

To repeat a value minimum number of times.

>>> :set -XDataKinds
>>> repeatLMin 123 :: RangeL 3 5 Integer
123 :. (123 :. (123 :. NilL))

-}

unfoldrMin ::
	(0 <= n, LoosenLMax n n m, Unfoldr 0 n n) => (s -> (a, s)) -> s -> RangeL n m a
unfoldrMin f = loosenLMax . unfoldr f

{-^

To evaluate a function to construct values minimum number of times.
The function recieve a state and return a value and a new state.

>>> :set -XDataKinds
>>> unfoldrMin (\n -> (n * 3, n + 1)) 1 :: RangeL 3 5 Integer
3 :. (6 :. (9 :. NilL))

-}

unfoldrMMin ::
	(0 <= n, Monad m, LoosenLMax n n w, Unfoldr 0 n n) => m a -> m (RangeL n w a)
unfoldrMMin f = loosenLMax <$> unfoldrM f

{-^

It is like @unfoldrMin@. But it use a monad instead of a function.

>>> :set -XDataKinds
>>> :module + Data.IORef
>>> r <- newIORef 1
>>> count = readIORef r >>= \n -> n * 3 <$ writeIORef r (n + 1)
>>> unfoldrMMin count :: IO (RangeL 3 5 Integer)
3 :. (6 :. (9 :. NilL))

-}

-- MAX

repeatLMax :: (0 <= m, LoosenLMin m m n, Unfoldr 0 m m) => a -> RangeL n m a
repeatLMax = unfoldrMax \x -> (x, x)

{-^

To repeat a value maximum number of times.

>>> :set -XDataKinds
>>> repeatLMax 123 :: RangeL 3 5 Integer
123 :. (123 :. (123 :. (123 :.. (123 :.. NilL))))

-}

unfoldrMax ::
	(0 <= m, LoosenLMin m m n, Unfoldr 0 m m) => (s -> (a, s)) -> s -> RangeL n m a
unfoldrMax f = loosenLMin . unfoldr f

{-^

To evaluate a function to construct values maximum number of times.
The function recieve a state and return a value and a new state.

>>> :set -XDataKinds
>>> unfoldrMax (\n -> (n * 3, n + 1)) 1 :: RangeL 3 5 Integer
3 :. (6 :. (9 :. (12 :.. (15 :.. NilL))))

-}

unfoldrMMax ::
	(0 <= w, Monad m, LoosenLMin w w n, Unfoldr 0 w w) => m a -> m (RangeL n w a)
unfoldrMMax f = loosenLMin <$> unfoldrM f

{-^

It is like @unfoldrMax@. But it use a monad instead of a function.

>>> :set -XDataKinds
>>> :module + Data.IORef
>>> r <- newIORef 1
>>> count = readIORef r >>= \n -> n * 3 <$ writeIORef r (n + 1)
>>> unfoldrMMax count :: IO (RangeL 3 5 Integer)
3 :. (6 :. (9 :. (12 :.. (15 :.. NilL))))

-}

---------------------------------------------------------------------------
-- RANGED LIST RIGHT
---------------------------------------------------------------------------

-- MIN

repeatRMin :: (0 <= n, LoosenRMax n n m, Unfoldl 0 n n) => a -> RangeR n m a
repeatRMin = unfoldlMin \x -> (x, x)

{-^

To repeat a value minimum number of times.

>>> :set -XDataKinds
>>> repeatRMin 123 :: RangeR 3 5 Integer
((NilR :+ 123) :+ 123) :+ 123

-}

unfoldlMin ::
	(0 <= n, LoosenRMax n n m, Unfoldl 0 n n) => (s -> (s, a)) -> s -> RangeR n m a
unfoldlMin f = loosenRMax . unfoldl f

{-^

To evaluate a function to construct values minimum number of times.
The function recieves a state and return a value and a new state.

>>> :set -XDataKinds
>>> unfoldlMin (\n -> (n + 1, n * 3)) 1 :: RangeR 3 5 Integer
((NilR :+ 9) :+ 6) :+ 3

-}

unfoldlMMin ::
	(0 <= n, Monad m, LoosenRMax n n w, Unfoldl 0 n n) => m a -> m (RangeR n w a)
unfoldlMMin f = loosenRMax <$> unfoldlM f

{-^

It is like @unfoldlMax@. But it uses a monad instead of a function.

>>> :set -XDataKinds
>>> :module + Data.IORef
>>> r <- newIORef 1
>>> count = readIORef r >>= \n -> n * 3 <$ writeIORef r (n + 1)
>>> unfoldlMMin count :: IO (RangeR 3 5 Integer)
((NilR :+ 9) :+ 6) :+ 3

-}

-- MAX

repeatRMax :: (0 <= m, LoosenRMin m m n, Unfoldl 0 m m) => a -> RangeR n m a
repeatRMax = unfoldlMax \x -> (x, x)

{-^

To repeat a value maximum number of times.

>>> :set -XDataKinds
>>> repeatRMax 123 :: RangeR 3 5 Integer
((((NilR :++ 123) :++ 123) :+ 123) :+ 123) :+ 123

-}

unfoldlMax ::
	(0 <= m, LoosenRMin m m n, Unfoldl 0 m m) => (s -> (s, a)) -> s -> RangeR n m a
unfoldlMax f = loosenRMin . unfoldl f

{-^

To eveluate a function to construct values maximum number of times.
The function recieves a state and return a value and a new state.

>>> :set -XDataKinds
>>> unfoldlMax (\n -> (n + 1, n * 3)) 1 :: RangeR 3 5 Integer
((((NilR :++ 15) :++ 12) :+ 9) :+ 6) :+ 3

-}

unfoldlMMax ::
	(0 <= w, Monad m, LoosenRMin w w n, Unfoldl 0 w w) => m a -> m (RangeR n w a)
unfoldlMMax f = loosenRMin <$> unfoldlM f

{-^

It is like @unfoldlMax@. But it uses a monad instead of function.

>>> :set -XDataKinds
>>> :module + Data.IORef
>>> r <- newIORef 1
>>> count = readIORef r >>= \n -> n * 3 <$ writeIORef r (n + 1)
>>> unfoldlMMax count :: IO (RangeR 3 5 Integer)
((((NilR :++ 15) :++ 12) :+ 9) :+ 6) :+ 3

-}

---------------------------------------------------------------------------
-- LEFT TO RIGHT
---------------------------------------------------------------------------

-- CLASS

infixl 5 ++.+

class LeftToRight n m v w where
	(++.+) :: RangeR n m a -> RangeL v w a -> RangeR (n + v) (m + w) a

	{-^

	To concatenate a right-list and a left-list and return a right-list.

	>>> :set -XDataKinds
	>>> sampleLeftToRight1 = NilR :++ 'f' :++ 'o' :+ 'o' :: RangeR 1 4 Char
	>>> sampleLeftToRight2 = 'b' :. 'a' :. 'r' :.. NilL :: RangeL 2 3 Char
	>>> sampleLeftToRight1 ++.+ sampleLeftToRight2
	(((((NilR :++ 'f') :++ 'o') :++ 'o') :+ 'b') :+ 'a') :+ 'r'
	>>> :type sampleLeftToRight1 ++.+ sampleLeftToRight2
	sampleLeftToRight1 ++.+ sampleLeftToRight2 :: RangeR 3 7 Char

	-}

-- INSTANCE

instance LeftToRight n m 0 0 where n ++.+ _ = n

instance {-# OVERLAPPABLE #-} (
	1 <= n, 1 <= m + 1, PushR (n - 1) m, LoosenRMax n m (m + w),
	LeftToRight n (m + 1) 0 (w - 1) ) => LeftToRight n m 0 w where
	(++.+) :: forall a . RangeR n m a -> RangeL 0 w a -> RangeR n (m + w) a
	(++.+) n = \case NilL -> loosenRMax n; x :.. v -> (n .:++ x :: RangeR n (m + 1) a) ++.+ v

instance {-# OVERLAPPABLE #-} (
	1 <= n + 1, 1 <= m + 1, 1 <= v,
	LeftToRight (n + 1) (m + 1) (v - 1) (w - 1)) =>
	LeftToRight n m v w where
	(++.+) :: forall a .
		RangeR n m a -> RangeL v w a -> RangeR (n + v) (m + w) a
	n ++.+ x :. v = (n :+ x :: RangeR (n + 1) (m + 1) a) ++.+ v

-- FUNCTION

leftToRight ::
	forall v w a . LeftToRight 0 0 v w => RangeL v w a -> RangeR v w a
leftToRight = ((NilR :: RangeR 0 0 a) ++.+)

{-^

To convert a left-list to a right-list.

>>> :set -XDataKinds -fno-warn-tabs
>>> :{
	sampleLeftToRight :: RangeL 3 8 Char
	sampleLeftToRight = 'h' :. 'e' :. 'l' :. 'l' :.. 'o' :.. NilL
:}

>>> leftToRight sampleLeftToRight
((((NilR :++ 'h') :++ 'e') :+ 'l') :+ 'l') :+ 'o'

-}

---------------------------------------------------------------------------
-- RIGHT TO LEFT
---------------------------------------------------------------------------

-- CLASS

infixr 5 ++..

class RightToLeft n m v w where
	(++..) :: RangeR n m a -> RangeL v w a -> RangeL (n + v) (m + w) a

	{-^

	To concatenate a right-list and a left-list and return a left-list.

	>>> :set -XDataKinds
	>>> sampleRightToLeft1 = NilR :++ 'f' :++ 'o' :+ 'o' :: RangeR 1 4 Char
	>>> sampleRightToLeft2 = 'b' :. 'a' :. 'r' :.. NilL :: RangeL 2 3 Char
	>>> sampleRightToLeft1 ++.. sampleRightToLeft2
	'f' :. ('o' :. ('o' :. ('b' :.. ('a' :.. ('r' :.. NilL)))))

	-}

-- INSTANCE

instance RightToLeft 0 0 v w where _ ++.. v = v

instance {-# OVERLAPPABLE #-} (
	1 <= v, 1 <= w + 1, PushL (v - 1) w, LoosenLMax v w (m + w),
	RightToLeft 0 (m - 1) v (w + 1) ) => RightToLeft 0 m v w where
	(++..) :: forall a . RangeR 0 m a -> RangeL v w a -> RangeL v (m + w) a
	NilR ++.. l = loosenLMax l
	(n :++ x) ++.. l = n ++.. (x .:.. l :: RangeL v (w + 1) a)

instance {-# OVERLAPPABLE #-} (
	1 <= n, 1 <= v + 1, 1 <= w + 1,
	RightToLeft (n - 1) (m - 1) (v + 1) (w + 1) ) =>
	RightToLeft n m v w where
	(++..) :: forall a .
		RangeR n m a -> RangeL v w a -> RangeL (n + v) (m + w) a
	n :+ x ++.. v = n ++.. (x :. v :: RangeL (v + 1) (w + 1) a)

-- FUNCTION

rightToLeft ::
	forall n m a . RightToLeft n m 0 0 => RangeR n m a -> RangeL n m a
rightToLeft = (++.. (NilL :: RangeL 0 0 a))

{-^

To convert a right-list to a left-list.

>>> :set -XDataKinds
>>> :{
	sampleRightToLeft :: RangeR 3 8 Char
	sampleRightToLeft = NilR :++ 'h' :++ 'e' :+ 'l' :+ 'l' :+ 'o'
:}

>>> rightToLeft sampleRightToLeft
'h' :. ('e' :. ('l' :. ('l' :.. ('o' :.. NilL))))

-}
