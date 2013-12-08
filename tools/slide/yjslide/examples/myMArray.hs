{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

class Indexable a e where
	index :: a e -> Int -> e

newtype List e = List [e]

instance Indexable List e where
	index (List lst) = (lst !!)
