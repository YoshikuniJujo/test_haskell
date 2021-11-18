{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}
-- {-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=Plugin.TypeCheck.Nat.Simple #-}

module Lib where

import GHC.TypeNats
import Data.List.Length

class Inner k where
	inner :: Num n => LengthL k n -> LengthL k n -> n

instance Inner 0 where inner _ _ = 0

instance {-# OVERLAPPABLE #-} Inner (k - 1) => Inner k where
	inner (x :. xs) (y :. ys) = x * y + inner xs ys
	inner NilL NilL = error "never occur"

class MultiInner k j where
	multiInner ::
		Num n => LengthL k n -> LengthL k (LengthL j n) -> LengthL j n

instance MultiInner k 0 where
	multiInner _ _ = NilL

instance {-# OVERLAPPABLE #-}
	(Inner k, MultiInner k (j - 1), 1 <= j, 1 <= k, Functor (LengthL k)) => MultiInner k j where
	multiInner v m = inner v r0 :. multiInner v rs
		where (r0, rs) = mapUncons m

uncons :: 1 <= i => LengthL i a -> (a, LengthL (i - 1) a)
uncons (x :. xs) = (x, xs)

unzipLengthL :: LengthL i (a, b) -> (LengthL i a, LengthL i b)
unzipLengthL NilL = (NilL, NilL)
unzipLengthL ((x, y) :. xys) = (x :. xs, y :. ys)
	where (xs, ys) = unzipLengthL xys

mapUncons :: (Functor (LengthL i), 1 <= j) =>
	LengthL i (LengthL j a) -> (LengthL i a, LengthL i (LengthL (j - 1) a))
mapUncons = unzipLengthL . (uncons <$>)

class Cross i k j where
	cross :: Num n => LengthL i (LengthL k n) ->
		LengthL k (LengthL j n) -> LengthL i (LengthL j n)

instance Cross 0 k j where cross _ _ = NilL

instance {-# OVERLAPPABLE #-} (
	1 <= i, Cross (i - 1) k j, MultiInner k j ) =>
	Cross i k j where
	cross (r :. rs) m = multiInner r m :. cross rs m

m1 :: LengthL 2 (LengthL 3 Int)
m1 =	(1 :. 2 :. 3 :. NilL) :.
	(4 :. 5 :. 6 :. NilL) :. NilL

m2 :: LengthL 3 (LengthL 4 Int)
m2 =	(1 :. 2 :. 3 :. 4 :. NilL) :.
	(5 :. 6 :. 7 :. 8 :. NilL) :.
	(9 :. 10 :. 11 :. 12 :. NilL) :. NilL
