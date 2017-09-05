{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures, DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MyEff (
	Eff, fromUnion, toUnion, castUnion, run, mkRun ) where

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

mkRun :: (Functor t, Typeable t) =>
	(a -> b) ->
	(t (Eff (t :> es) a) -> b -> b) ->
	(t (Eff (t :> es) a) -> Eff (t :> es) a) ->
	Eff (t :> es) a -> Eff es b
mkRun p m h f = case f of
	Pure x -> Pure $ p x
	Free u -> case fromUnion u of
		Just t -> m t <$> mkRun p m h (h t)
		Nothing -> Free . fmap (mkRun p m h) $ castUnion u
