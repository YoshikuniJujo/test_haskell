{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

import Prelude hiding (flip)

class Hoge a where
	hoge :: a -> (a, a)

instance Hoge (Either b b) where
	hoge (Left x) = (Left x, Right x)
	hoge (Right x) = (Left x, Right x)

instance Hoge (Maybe Char) where
	hoge (Just c) = (Just c, Nothing)
	hoge Nothing = (Just ' ', Nothing)

instance Hoge String where
	hoge s = (s, reverse s)

instance Hoge [Int] where
	hoge is = (is, [product is])

instance Hoge (Either String Int) where
	hoge (Left str) = (Left str, Right $ length str)
	hoge (Right i) = (Left $ show i, Right i)

class Flip a where
	flip :: a -> a

instance Flip (Either a a) where
	flip (Left x) = Right x
	flip (Right x) = Left x

instance Flip (a, a) where
	flip (x, y) = (y, x)

class Rev a where
	rev :: a -> a

instance Rev (Either a a) where
	rev (Left x) = Right x
	rev (Right x) = Left x

instance Rev (a, a) where
	rev (x, y) = (y, x)

instance Rev (a, b, a) where
	rev (x, y, z) = (z, y, x)

instance Rev (a, b, b, a) where
	rev (x, y, z, w) = (w, z, y, x)

instance Rev [a] where
	rev = reverse

isPalindrome :: (Rev a, Eq a) => a -> Bool
isPalindrome x = x == rev x
