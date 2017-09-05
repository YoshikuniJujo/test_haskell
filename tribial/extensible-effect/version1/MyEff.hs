{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures, DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MyEff (
	Eff, fromUnion, toUnion, castUnion, run ) where

import Data.Typeable

import Free
import Cast
import TypeLevel

type Eff es = Free (Union es)

data Union (es :: Effs) a = forall t . (Functor t, Typeable t) => U (t a)

instance Functor (Union es) where
	fmap f (U ta) = U $ fmap f ta

toUnion :: (Functor t, Typeable t) => t a -> Union es a
toUnion = U

fromUnion :: (Functor t, Typeable t) => Union es a -> Maybe (t a)
fromUnion (U ta) = cast1 ta

castUnion :: Union es a -> Union es' a
castUnion (U ta) = U ta

run :: Free (Union Base) a -> a
run (Pure x) = x
run _ = error "MyEff.run: never occur"
