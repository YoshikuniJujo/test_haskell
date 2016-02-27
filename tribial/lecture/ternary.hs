data Ternary = Ternary Bool [Tri]

instance Show Ternary where
	show (Ternary s ts) =
		(if s then "-" else "") ++ "0t" ++ concatMap show (reverse ts)

data Tri = Zero | One | Two

instance Show Tri where
	show Zero = "0"
	show One = "1"
	show Two = "2"

toI :: Ternary -> Integer
toI (Ternary s ts) = (if s then negate else id) $ ti ts
	where
	ti [] = 0
	ti (Zero : ts) = 3 * ti ts
	ti (One : ts) = 1 + 3 * ti ts
	ti (Two : ts) = 2 + 3 * ti ts

fromI :: Integer -> Ternary
fromI n	= Ternary (n < 0) . fi $ abs n
	where
	fi 0 = []
	fi n = case n `mod` 3 of
		0 -> Zero : fi (n `div` 3)
		1 -> One : fi (n `div` 3)
		2 -> Two : fi (n `div` 3)
		_ -> error "never occur"

instance Num Ternary where
	t1 + t2 = fromI $ toI t1 + toI t2
	t1 * t2 = fromI $ toI t1 * toI t2
	negate (Ternary s t) = Ternary (not s) t
	abs (Ternary _ t) = Ternary False t
	signum (Ternary _ []) = Ternary False []
	signum (Ternary s _) = Ternary s [One]
	fromInteger = fromI
