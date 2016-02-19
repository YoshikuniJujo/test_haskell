data LL a = Nil | Cons (LL a) a

{-
instance Show a => Show (LL a) where
	show (Cons ll x) = "Cons (" ++ show ll ++ ") " ++ show x
	show _ = "Nil"
	-}

instance Show a => Show (LL a) where
	showsPrec _ (Cons ll x) =
		("Cons (" ++) . showsPrec 11 ll . (") " ++) . showsPrec 11 x
	showsPrec _ _ = ("Nil" ++)

{-
instance Show a => Show (LL a) where
	showsPrec d (Cons ll x) = showParen (d > 10) $
		("Cons " ++) . showsPrec 11 ll . (" " ++) . showsPrec 11 x
	showsPrec _ _ = ("Nil" ++)
	-}
