{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import System.Random

newtype Cont r a = Cont { runCont :: (a -> r) -> r }

instance Monad (Cont r) where
	return a = Cont ($ a)			-- \k -> k a
	Cont c >>= f = Cont $ \k -> c $ (`runCont` k) . f
						-- \k -> c $ (\a -> f a k)

data VE s w = V w | S (State s (VE s w))

data State s w = State (s -> s) (s -> w)

modifyGet :: (s -> s) -> Cont (VE s w) s
modifyGet f = Cont $ S . State f

modify :: (s -> s) -> Cont (VE s w) ()
modify f = modifyGet f >> return ()

get :: Cont (VE s w) s
get = modifyGet id

put :: s -> Cont (VE s w) ()
put = modify . const

runState :: Cont (VE s a) a -> s -> a
runState m = sloop (runCont m V)

sloop :: VE s a -> s -> a
sloop m s = case m of
	V x -> x
	S (State f k) -> sloop (k s) (f s)

getAny :: (Random a) => Cont (VE StdGen w) a
getAny = do
	g <- get
	let (x, g') = random g
	put g'
	return x

mkRandomValue :: StdGen -> (Int, Bool)
mkRandomValue = runState $ do
	n <- getAny
	b <- getAny
	return (n, b)

instance Functor (Cont r) where
	fmap = (=<<) . (return .)

instance Applicative (Cont r) where
	pure = return
	mf <*> mx = do f <- mf; x <- mx; return $ f x
