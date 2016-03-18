{-# LANGUAGE MonadComprehensions #-}

import Control.Applicative
import Data.Char (ord)

data Logger a = Logger [String] a deriving Show

toCode :: Char -> Logger Int
toCode c = Logger ["toCode " ++ show c] (ord c)

tell :: String -> Logger ()
tell l = Logger [l] ()

toCode' :: Char -> Logger Int
toCode' c = tell ("toCode " ++ show c) >> return (ord c)

double :: Int -> Logger Int
double n = tell ("double " ++ show n) >> return (n * 2)

toCodeDouble :: Char -> Logger Int
toCodeDouble c = toCode c >>= double

instance Functor Logger where
	fmap = (=<<) . (return .)

instance Applicative Logger where
	pure = return
	mf <*> mx = [ f x | f <- mf, x <- mx ]

instance Monad Logger where
	return = Logger []
	Logger ls x >>= f = let Logger ls' y = f x in Logger (ls ++ ls') y
