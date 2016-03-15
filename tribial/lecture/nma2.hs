import Control.Applicative

data AppSum a = AppSum Int deriving Show

instance Functor AppSum where
	fmap _ (AppSum n) = AppSum n

instance Applicative AppSum where
	pure = const (AppSum 0)
	AppSum n <*> AppSum n' = AppSum $ n + n'
