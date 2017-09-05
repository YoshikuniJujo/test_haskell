-- {-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification, DeriveFunctor #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.Typeable

import Free
import Cast

data Union a = forall t . (Functor t, Typeable t) => U (t a)

instance Functor Union where
	fmap f (U ta) = U $ fmap f ta

toUnion :: (Functor t, Typeable t) => t a -> Union a
toUnion = U

fromUnion :: (Functor t, Typeable t) => Union a -> Maybe (t a)
fromUnion (U ta) = cast1 ta

newtype Reader r w = Reader (r -> w) deriving Functor

ask :: Typeable r => Free Union r
ask = Free . U $ Reader Pure

run :: Free Union a -> a
run (Pure x) = x

runReader :: Typeable r => Free Union a -> r -> a
runReader f r = case f of
	Pure x -> x
	Free u -> case fromUnion u of
		Just (Reader f') -> runReader (f' r) r
		Nothing -> undefined

runReader2 :: Typeable r => Free Union a -> r -> Free Union a
runReader2 f r = case f of
	Pure x -> Pure x
	Free u -> case fromUnion u of
		Just (Reader f') -> runReader2 (f' r) r
		Nothing -> Free $ fmap (`runReader2` r) u
