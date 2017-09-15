{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

infix 4 :==:
infixl 6 :+:
infixl 7 :*:

data Expr a
	= I Int
	| B Bool
	| Expr a :+: Expr a
	| Expr a :*: Expr a
	| Expr a :==: Expr a
	deriving Show

i :: Int -> Expr Int
i = I

b :: Bool -> Expr Bool
b = B

(.+.), (.*.) :: Expr Int -> Expr Int -> Expr Int
(.+.) = (:+:)
(.*.) = (:*:)

-- (.==.) :: Expr Int -> Expr Int -> Expr Bool
-- (.==.) = (:==:)

-- eval :: Expr a -> a
-- eval (I n) = n
