data Rep a = Rep Int a deriving Show

toList :: Rep a -> [a]
toList (Rep n x) = replicate n x
