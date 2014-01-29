{-# LANGUAGE TupleSections #-}

import Data.Char

data Logger a = Logger [String] a deriving Show

instance Monad Logger where
	return = Logger []
	Logger l x >>= f = let Logger l' x' = f x in Logger (l ++ l') x'

tell :: String -> Logger ()
tell l = Logger [l] ()

example, example', example'' :: Logger Int
example = do
	tell "x = 3 + 4"
	x <- return $ 3 + 4
	tell "y = x * 8"
	y <- return $ x * 8
	tell "return y"
	return y

example' = do
	tell "x = 3 + 4"
	let x = 3 + 4
	tell "y = x * 8"
	let y = x * 8
	tell "return y"
	return y

example'' =
	tell "x = 3 + 4" >>
	return (3 + 4) >>= \x ->
	tell "y = x * 8" >>
	return (x * 8) >>= \y ->
	tell "return y" >>
	return y

--
-- m >>= (\x -> f x >>= g)
-- (m >>= \x -> f x) >>= g
--

toCode :: Char -> Logger Int
toCode c = Logger ["toCode " ++ show c] (ord c)

double :: Int -> Logger Int
double i = Logger ["double " ++ show i] (i * 2)
