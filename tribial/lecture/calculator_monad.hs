{-# LANGUAGE MonadComprehensions #-}

import Control.Monad

-- (3 * 4 + 2 * 5) * 7
--
-- 3 * 4 M+ C 2 * 5 M+ C MR * 7
--

newtype State a = State { runState :: Int -> (a, Int) }

instance Functor State where
	fmap = (=<<) . (return .)

instance Applicative State where
	pure = return
	mf <*> mx = [ f x | f <- mf, x <- mx ]

instance Monad State where
	State m >>= f = State $ \s -> let (x, s') = m s in runState (f x) s'
	return = State . (,)

get :: State Int
get = State $ \s -> (s, s)

put :: Int -> State ()
put x = State $ \_ -> ((), x)

modify :: (Int -> Int) -> State ()
modify f = get >>= put . f

madd :: Int -> State ()
madd x = modify (+ x)

example1 :: State Int
example1 = do
	madd $ 3 * 4
	madd $ 2 * 5
	(* 7) <$> get

example2 :: State [Int]
example2 = forM [9, 2, 5, 8, 4, 3] $ \x -> do
	madd $ x `div` 4
	madd $ x `mod` 4
	return $ x * 3

example3 :: State ()
example3 = forM_ [9, 2, 5, 8, 4, 3] $ \x -> do
	madd $ x `div` 4
	madd $ x `mod` 4
	return $ x * 3
