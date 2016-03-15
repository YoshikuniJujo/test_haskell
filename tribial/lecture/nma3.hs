import Control.Applicative

data NMA a = Id a | Num Int deriving Show

instance Functor NMA where
	fmap f (Id x) = Id $ f x
	fmap _ (Num n) = Num n

instance Applicative NMA where
	pure = Id
	Id f <*> Id x = Id $ f x
	Num n <*> Num n' = Num $ n + n'
	Num n <*> _ = Num n
	_ <*> Num n' = Num n'
