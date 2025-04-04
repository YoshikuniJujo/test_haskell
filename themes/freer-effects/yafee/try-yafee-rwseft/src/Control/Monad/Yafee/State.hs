{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yafee.State where

import Control.Monad.Fix
import Control.Monad.Yafee.Eff qualified as Eff
import Control.Monad.Freer qualified as Freer
import Control.OpenUnion qualified as Union
import Data.FTCQueue qualified as FTCQueue

data State s a where Get :: State s s; Put :: !s -> State s ()

get :: Union.Member (State s) effs => Eff.E effs s
get = Eff.eff Get

gets :: Union.Member (State s) effs => (s -> t) -> Eff.E effs t
gets f = f <$> get

put :: Union.Member (State s) effs => s -> Eff.E effs ()
put = Eff.eff . Put

modify :: Union.Member (State s) effs => (s -> s) -> Eff.E effs ()
modify f = put . f =<< get

runState :: Eff.E (State s ': effs) a -> s -> Eff.E effs (a, s)
Freer.Pure x `runState` s = pure (x, s)
(u Freer.:>>= q) `runState` s = case Union.decomp u of
	Left u' -> u' Freer.:>>=
		FTCQueue.singleton ((`runState` s) `Freer.comp` q)
	Right Get -> q `Freer.app` s `runState` s
	Right (Put s') -> q `Freer.app` () `runState` s'

transactionState :: forall s effs a .
	Union.Member (State s) effs => Eff.E effs a -> Eff.E effs a
transactionState m = do
	(s0 :: s) <- get
	($ m) . ($ s0) $ fix \go s -> \case
		Freer.Pure x -> put s >> pure x
		u Freer.:>>= q -> case Union.prj @(State s) u of
			Nothing -> u Freer.:>>=
				FTCQueue.singleton (go s `Freer.comp` q)
			Just Get -> go s $ q `Freer.app` s
			Just (Put s') -> go s' $ q `Freer.app` ()
