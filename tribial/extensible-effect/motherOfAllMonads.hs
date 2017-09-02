{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Arrow
import Control.Monad.Cont

i :: Monad m => m a -> Cont (m b) a
i x = cont (\fred -> x >>= fred)

j :: Monad m1 => m1 a -> Cont (m1 (m2 c)) a
j x = cont (\fred -> x >>= fred)

-- k :: Monad m2 => m2 a -> Cont (m1 (m2 c)) a

run :: Monad m => Cont (m a) a -> m a
run m = runCont m return

test10 :: IO ()
test10 = run $ do
	i $ print "What is your name?"
	name <- i getLine
	i $ print $ "Merry Xmas " ++ name

some :: [Integer]
some = run $ do
	x <- i [3 :: Integer, 5, 7]
	y <- i [2, 1]
	return $ x * y

hoge = do
--	x <- i $ [3 :: Integer, 5, 7]
	j $ print "What is your name?"
	return [5]
--	return x
