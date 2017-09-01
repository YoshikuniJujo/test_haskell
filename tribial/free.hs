{-# LANGUAGE GADTs, Rank2Types #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Free

data Free f a = Pure a | Free (f (Free f a))

instance Functor f => Functor (Free f) where
	fmap f (Pure a) = Pure (f a)
	fmap f (Free fa) = Free (fmap (fmap f) fa)

instance Functor f => Applicative (Free f) where
	pure = Pure
	mf <*> mx = do f <- mf; x <- mx; return $ f x

instance Functor f => Monad (Free f) where
	return = Pure
	(>>=) = flip $ (flatten .) . fmap
{-
	Pure a >>= k = k a
	Free fm >>= k = Free (fmap (>>= k) fm)
-}

liftF :: Functor f => f a -> Free f a
liftF f = Free $ fmap Pure f

-- flatten

flatten :: Functor f => Free f (Free f a) -> Free f a
flatten (Pure f) = f
flatten (Free f) = Free $ fmap flatten f

-- example

data CharIO a = GetCh (Char -> a) | PutCh Char a

instance Functor CharIO where
	fmap f (GetCh g) = GetCh (f . g)
	fmap f (PutCh c x) = PutCh c (f x)

getCh :: Free CharIO Char
getCh = Free $ GetCh $ \ch -> Pure ch

putCh :: Char -> Free CharIO ()
putCh ch = Free $ PutCh ch (Pure ())

runStdIO :: Free CharIO a -> IO a
runStdIO (Pure a) = return a
runStdIO (Free (GetCh f)) = getChar >>= \ch -> runStdIO (f ch)
runStdIO (Free (PutCh ch cont)) = putChar ch >> runStdIO cont

testIO :: Free CharIO ()
testIO = do
	ch <- getCh
	putCh ch
	putCh '\n'

-- example2

some :: Free Maybe Int
some = do
	x <- Free $ Just $ Pure 3
	y <- Free $ Just $ Pure 2
	return $ x + y

fromFreeMaybe :: Free Maybe a -> Maybe a
fromFreeMaybe (Pure x) = Just x
fromFreeMaybe (Free m) = fromFreeMaybe =<< m
