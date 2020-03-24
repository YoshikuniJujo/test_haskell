{-# LANGUAGE LambdaCase, EmptyCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module NanimoshitakuneMonad where

data Nihil a

instance Functor Nihil where
	fmap _ = \case

instance Applicative Nihil where
	pure _ = error "nihil ad facere,"
	(<*>) = \case

instance Monad Nihil where
	(>>=) = \case
