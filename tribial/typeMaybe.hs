{-# LANGUAGE GADTs, TypeFamilies, DataKinds #-}

data Nat = Z | S Nat

data Vec :: Nat -> * where
	Nil :: Vec Z
	Cons :: Int -> Vec n -> Vec (S n)

some :: Vec (S (S (S (S Z))))
some = 5 `Cons` (8 `Cons` (9 `Cons` (10 `Cons` Nil)))
