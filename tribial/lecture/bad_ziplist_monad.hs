import Control.Applicative

newtype MyZipList a = MZL { unMZL :: [a] } deriving Show

instance Functor MyZipList where
	fmap f = MZL . fmap f . unMZL

instance Applicative MyZipList where
--	pure = MZL . (: [])
--	MZL fs <*> MZL xs = MZL $ zipWith ($) fs xs
	pure = return
	fs <*> xs = do
		f <- fs
		x <- xs
		return $ f x

-- It violate monad law.

instance Monad MyZipList where
	return = MZL . repeat
	xs >>= f = myJoin $ fmap f xs

myJoin :: MyZipList (MyZipList a) -> MyZipList a
myJoin (MZL []) = MZL []
myJoin (MZL xss) | any null $ map unMZL xss = MZL []
myJoin (MZL (MZL (x : xs) : xss)) = MZL $
	x : unMZL (myJoin . MZL $ map (MZL . tail . unMZL) xss)
