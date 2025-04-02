{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module FreeMonads where

import Control.Arrow

import Free

data Reader e a = Reader (e -> a)

instance Functor (Reader e) where f `fmap` Reader k = Reader $ f . k

ask :: Free (Reader e) e
ask = Join $ Reader Pure

runReader :: Free (Reader e) a -> e -> a
runReader m e = case m of Pure x -> x; Join (Reader k) -> runReader (k e) e

data Writer w a = Writer w a

instance Functor (Writer w) where f `fmap` Writer w a = Writer w $ f a

tell :: w -> Free (Writer w) ()
tell w = Join . Writer w $ Pure ()

runWriter :: Monoid w => Free (Writer w) a -> (a, w)
runWriter = \case
	Pure x -> (x, mempty)
	Join (Writer w m) -> second (w <>) $ runWriter m
