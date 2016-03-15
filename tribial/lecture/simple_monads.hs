import Control.Applicative

data Empty a = Empty deriving Show

instance Functor Empty where
	fmap _ _ = Empty

instance Applicative Empty where
	pure = const Empty
	_ <*> _ = Empty

instance Monad Empty where
	return = const Empty
	_ >>= _ = Empty
