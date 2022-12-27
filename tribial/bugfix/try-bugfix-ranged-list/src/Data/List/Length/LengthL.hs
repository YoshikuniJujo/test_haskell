{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.List.Length.LengthL (
	LengthL, unfoldr, unfoldrWithBase, unfoldrM, unfoldrMWithBase,
	ListToLengthL, splitL ) where

import GHC.TypeNats (type (-), type (<=))
import Control.Arrow (first, (+++))
import Control.Monad.State (StateL(..))
import Data.List.Range.RangeL (LengthL, RangeL(..), Unfoldr, unfoldrMRangeWithBase)

---------------------------------------------------------------------------

-- * TYPE
-- * UNFOLDR
-- * LIST TO LENGTH LEFT

---------------------------------------------------------------------------
-- TYPE
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- UNFOLDR
---------------------------------------------------------------------------

unfoldr :: (0 <= n, Unfoldr 0 n n) => (s -> (a, s)) -> s -> LengthL n a
unfoldr = unfoldrWithBase NilL

{-^

To evaluate function repeatedly to construct a list of type @LengthL n a@.
The function recieve a state and return an element value and a new state.

>>> :set -XDataKinds
>>> unfoldr (\n -> (2 * n, n + 1)) 0 :: LengthL 5 Integer
0 :. (2 :. (4 :. (6 :. (8 :. NilL))))

-}

unfoldrWithBase ::
	Unfoldr n m m => RangeL n m a -> (s -> (a, s)) -> s -> LengthL m a
unfoldrWithBase xs = (fst .) . runStateL . unfoldrMWithBase xs . StateL

{-^

It is like @unfoldr@. But it has already prepared values.

>>> :set -XDataKinds
>>> xs = 123 :. 456 :.. NilL :: RangeL 1 5 Integer
>>> unfoldrWithBase xs (\n -> (2 * n, n + 1)) 0 :: LengthL 5 Integer
123 :. (456 :. (0 :. (2 :. (4 :. NilL))))

-}

unfoldrM :: (Monad m, 0 <= n, Unfoldr 0 n n) => m a -> m (LengthL n a)
unfoldrM = unfoldrMWithBase NilL

{-^

It is like @unfoldr@. But it use a monad as an argument instead of a function.

>>> :set -XDataKinds
>>> :module + Data.IORef
>>> r <- newIORef 1
>>> count = readIORef r >>= \n -> n <$ writeIORef r (n +1)
>>> unfoldrM count :: IO (LengthL 5 Integer)
1 :. (2 :. (3 :. (4 :. (5 :. NilL))))

-}

unfoldrMWithBase ::
	(Monad m, Unfoldr n w w) => RangeL n w a -> m a -> m (LengthL w a)
unfoldrMWithBase = (`unfoldrMRangeWithBase` undefined)

{-^

It is like @unfoldrM@. But it has already prepared values.

>>> :set -XDataKinds
>>> :module + Data.IORef
>>> r <- newIORef 1
>>> count = readIORef r >>= \n -> n <$ writeIORef r (n + 1)
>>> unfoldrMWithBase (123 :. 456 :.. NilL) count :: IO (LengthL 5 Integer)
123 :. (456 :. (1 :. (2 :. (3 :. NilL))))

-}

---------------------------------------------------------------------------
-- LIST TO LENGTH LEFT
---------------------------------------------------------------------------

class ListToLengthL n where
	splitL :: [a] -> Either (RangeL 0 (n - 1) a) (LengthL n a, [a])

	{-^

	To take a lengthed list from a list.
	If an original list has not enough elements, then it return
	a left value.

	>>> :set -XTypeApplications -XDataKinds
	>>> splitL @4 "Hi!"
	Left ('H' :.. ('i' :.. ('!' :.. NilL)))
	>>> splitL @4 "Hello!"
	Right ('H' :. ('e' :. ('l' :. ('l' :. NilL))),"o!")

	-}

instance ListToLengthL 1 where
	splitL = \case [] -> Left NilL; x : xs -> Right (x :. NilL, xs)

instance {-# OVERLAPPABLE #-}
	(1 <= n, 1 <= (n - 1), 0 <= (n - 1), ListToLengthL (n - 1)) => ListToLengthL n where
	splitL = \case
		[] -> Left NilL
		x : xs -> (x :..) +++ ((x :.) `first`) $ splitL xs
