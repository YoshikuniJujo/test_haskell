{-# LANGUAGE ExistentialQuantification, DeriveFunctor #-}
{-# LANGUAGE KindSignatures, DataKinds, TypeOperators, FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

import Data.Typeable

import TypeLevel
import Free
import Cast

data Union (es :: Effs) a = forall t . (Functor t, Typeable t) => U (t a)

instance Functor (Union es) where
	fmap f (U ta) = U $ fmap f ta

toUnion :: (Functor t, Typeable t) => t a -> Union es a
toUnion = U

fromUnion :: (Functor t, Typeable t) => Union es a -> Maybe (t a)
fromUnion (U ta) = cast1 ta

castUnion :: Union es a -> Union es' a
castUnion (U ta) = U ta

run :: Free (Union Emp) a -> a
run (Pure x) = x
run _ = error "never occur"

newtype Reader r w = Reader (r -> w) deriving Functor

ask :: (Member (Reader r) es, Typeable r) => Free (Union es) r
ask = Free . U $ Reader Pure

runReader :: Typeable r =>
	Free (Union (Reader r :> es)) a -> r -> Free (Union es) a
runReader f r = case f of
	Pure x -> Pure x
	Free u -> case fromUnion u of
		Just (Reader f') -> runReader (f' r) r
		Nothing -> Free . fmap (`runReader` r) $ castUnion u
