{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ReaderWriter where

import Control.Arrow

import Free

data RW e w a = Reader (e -> a) | Writer w a

instance Functor (RW e w) where
	f `fmap` Reader k = Reader $ f . k
	f `fmap` Writer w x = Writer w $ f x

ask :: Free (RW e w) e
ask = Join $ Reader Pure

tell :: w -> Free (RW e w) ()
tell w = Join . Writer w $ Pure ()

sample :: Free (RW String String) (String, String)
sample = do
	x <- ask
	tell $ "I say " ++ x ++ ".\n"
	y <- ask
	tell $ "You say Good-bye!\n"
	return (x, y)

runRW :: Monoid w => Free (RW e w) a -> e -> (a, w)
runRW m e = case m of
	Pure x -> (x, mempty)
	Join (Reader k) -> runRW (k e) e
	Join (Writer w m') -> second (w <>) $ runRW m' e

runStateRW :: Free (RW s s) a -> s -> (a, s)
runStateRW m s = case m of
	Pure x -> (x, s)
	Join (Reader k) -> runStateRW (k s) s
	Join (Writer s' m') -> runStateRW m' s'
