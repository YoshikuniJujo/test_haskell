{-# LANGUAGE LambdaCase, ExistentialQuantification #-}
-- {-# LANGUAGE LambdaCase, ExistentialQuantification, RankNTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MyEff (
	(:>), Member, MemberU2, VE(..), run, fromEffect, toEffect
	) where

import Control.Monad.Cont (Cont, runCont)
import Data.Typeable (Typeable)
import Data.Void

import TypeElem ((:>), Member, MemberU2)
import Cast (cast1)

-- type Eff r = forall w . Cont (VE w r)

data VE r a
	= V a
	| forall t . (Functor t, Typeable t) => E (t (VE r a))
	deriving Typeable

toEffect :: (Functor t, Typeable t) => t (VE r a) -> VE r a
toEffect = E

fromEffect :: (Functor t, Typeable t) => VE r a -> Maybe (t (VE r a))
fromEffect = \case E e -> cast1 e; V _ -> Nothing

run :: Cont (VE Void a) a -> a
-- run :: Eff Void a -> a
run m = case runCont m V of V x -> x; _ -> undefined
