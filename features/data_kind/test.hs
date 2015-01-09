{-# LANGUAGE TypeOperators, FlexibleInstances #-}

data ZT = ZV
data tail :. head = tail :.. head deriving Show

data Hoge a b = Hoge a b deriving Show

class Hogable h where
	add :: h -> Int

instance Hogable () where
	add _ = 0

instance Hogable b => Hogable (Hoge Int b) where
	add (Hoge x h) = x + add h

instance Hogable b => Hogable (Int, b) where
	add (x, h) = x + add h
