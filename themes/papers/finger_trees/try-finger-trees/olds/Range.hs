{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE TypeApplications, AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Range where

import GHC.TypeLits

infixr 5 :., :..

data Range :: Nat -> Nat -> * -> * where
	Nil :: Range 0 0 a
	Gap :: Range 0 (m - 1) a -> Range 0 m a
	(:..) :: a -> Range 0 (m - 1) a -> Range 0 m a
	(:.) :: a -> Range (n - 1) (m - 1) a -> Range n m a

class Convert s t where
	convert :: s a -> t a

instance Convert (Range 0 0) (Range 0 0) where
	convert = id

instance {-# OVERLAPPABLE #-} Convert (Range 0 0) (Range 0 (m' - 1)) =>
	Convert (Range 0 0) (Range 0 m') where
	convert Nil = Gap $ convert Nil
	convert _ = error "never occur"

instance {-# OVERLAPPABLE #-} Convert (Range 0 (m - 1)) (Range 0 (m' - 1)) =>
	Convert (Range 0 m) (Range 0 m') where
	convert (Gap xs) = Gap $ convert xs
	convert (x :.. xs) = x :.. convert xs
	convert _ = error "never occur"

instance {-# OVERLAPPABLE #-} Convert (Range (n - 1) (m - 1)) (Range 0 (m' - 1)) =>
	Convert (Range n m) (Range 0 m') where
	convert (x :. xs) = x :.. convert xs
	convert _ = error "never occur"

instance {-# OVERLAPPABLE #-} Convert (Range (n - 1) (m - 1)) (Range (n' - 1) (m' - 1)) =>
	Convert (Range n m) (Range n' m') where
	convert (x :. xs) = x :. convert xs	
	convert _ = error "never occur"

class PushRange n m  where
--	pushRangeL :: a -> Range n m a -> Either (Range n m a) (Range (m + 1) (m + 1) a)
	pushRangeL :: a -> Range n m a -> Either (Range n m a) (Range n (m + 1) a)
--	pushRangeL :: m' ~ (m + 1) => a -> Range n m a -> Either (Range n m a) (Range m' m' a)

instance PushRange 0 0 where
	pushRangeL a Nil = Right $ a :.. Nil
	pushRangeL _ _ = error "never occur"

{-
instance PushRange 0 1 where
	pushRangeL a (Gap xs) = Left $ a :.. xs
	pushRangeL a (x :.. xs) = case pushRangeL x xs of
		Left ys -> Left $ a :.. ys
		Right ys -> Right $ a :. ys
		-}

instance {-# OVERLAPPABLE #-} PushRange 0 (m - 1) => PushRange 0 m where
	pushRangeL a (Gap xs) = Left $ a :.. xs
	pushRangeL a xa@(x :.. xs) = case pushRangeL x xs of
		Left ys -> Left $ a :.. ys
--		Right _ -> Right $ a :.. xa
--		Right xs -> Right $ a :. xs

class FromList t where
	fromList :: [a] -> t a

instance FromList (Range 0 0) where
	fromList [] = Nil
	fromList _ = error "bad"

instance {-# OVERLAPPABLE #-} FromList (Range 0 (m - 1)) => FromList (Range 0 m) where
	fromList [] = Gap (fromList [])
	fromList (x : xs) = x :.. fromList xs

instance {-# OVERLAPPABLE #-} FromList (Range (n - 1) (m - 1)) => FromList (Range n m) where
	fromList [] = error "bad"
	fromList (x : xs) = x :. fromList xs

deriving instance Show a => Show (Range n m a)

instance Foldable (Range 0 0) where
	foldr _ z Nil = z
	foldr _ _ _ = error "never occur"
	foldl _ z Nil = z
	foldl _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-} Foldable (Range 0 (m - 1)) => Foldable (Range 0 m) where
	foldr (-<) z (Gap xs) = foldr (-<) z xs
	foldr (-<) z (x :.. xs) = x -< foldr (-<) z xs
	foldr _ _ _ = error "never occur"
	foldl (>-) z (Gap xs) = foldl (>-) z xs
	foldl (>-) z (x :.. xs) = foldl (>-) (z >- x) xs
	foldl _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-} Foldable (Range (n - 1) (m - 1)) => Foldable (Range n m) where
	foldr (-<) z (x :. xs) = x -< foldr (-<) z xs
	foldr _ _ _ = error "never occur"
	foldl (>-) z (x :. xs) = foldl (>-) (z >- x) xs
	foldl _ _ _ = error "never occur"
