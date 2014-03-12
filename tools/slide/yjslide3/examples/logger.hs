data Logger a = Logger [String] a deriving Show

instance Monad Logger where
	return = Logger []
