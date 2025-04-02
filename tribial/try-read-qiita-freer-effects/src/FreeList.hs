{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module FreeList where

data FreeList a = PureL a | JoinL [FreeList a] deriving Show

mulAdd :: Integer -> Integer -> [Integer]
mulAdd y x = [x + y, x * y]

instance Functor FreeList where
	f `fmap` PureL x = PureL $ f x
	f `fmap` JoinL xs = JoinL $ fmap f `map` xs

instance Applicative FreeList where
	pure = PureL
	PureL f <*> mx = f <$> mx
	JoinL fs <*> mx = JoinL $ (<*> mx) `map` fs

instance Monad FreeList where
	PureL x >>= f = f x
	JoinL xs >>= f = JoinL $ (f =<<) `map` xs

mulAddF :: Integer -> Integer -> FreeList Integer
mulAddF y x = JoinL [PureL $ x + y, PureL $ x * y]

runList :: FreeList a -> [a]
runList (PureL x) = [x]
runList (JoinL xs) = runList `concatMap` xs

data FreeIO a
	= PureIO a
	| JoinIO (IO (FreeIO a))

instance Functor FreeIO where
	f `fmap` PureIO x = PureIO $ f x
	f `fmap` JoinIO m = JoinIO $ fmap f <$> m

instance Applicative FreeIO where
	pure = PureIO
	PureIO f <*> mx = f <$> mx
	JoinIO mf <*> mx = JoinIO $ (<*> mx) <$> mf

instance Monad FreeIO where
	PureIO x >>= f = f x
	JoinIO mx >>= f = JoinIO $ (f =<<) <$> mx

count :: Integer -> IO Integer
count n = putStrLn ("n = " ++ show n) >> return (n + 1)

countF :: Integer -> FreeIO Integer
countF n = JoinIO $ putStrLn ("n = " ++ show n) >> return (PureIO $ n + 1)

runIO :: FreeIO a -> IO a
runIO = \case PureIO x -> return x; JoinIO m -> m >>= runIO

runWith :: FreeIO a -> IO b -> IO a
runWith fio act = case fio of
	PureIO x -> return x
	JoinIO m -> act >> m >>= (`runWith` act)
