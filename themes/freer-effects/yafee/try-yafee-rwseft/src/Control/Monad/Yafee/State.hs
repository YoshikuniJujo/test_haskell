{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yafee.State (
	S(..), get, gets, put, modify, run, transaction ) where

import Control.Monad.Fix
import Control.Monad.Yafee.Eff qualified as Eff
import Control.Monad.Freer qualified as Freer
import Control.OpenUnion qualified as Union
import Data.FTCQueue qualified as FTCQueue

data S s a where Get :: S s s; Put :: !s -> S s ()

get :: Union.Member (S s) effs => Eff.E effs s
get = Eff.eff Get

gets :: Union.Member (S s) effs => (s -> t) -> Eff.E effs t
gets f = f <$> get

put :: Union.Member (S s) effs => s -> Eff.E effs ()
put = Eff.eff . Put

modify :: Union.Member (S s) effs => (s -> s) -> Eff.E effs ()
modify f = put . f =<< get

run :: Eff.E (S s ': effs) a -> s -> Eff.E effs (a, s)
Freer.Pure x `run` s = pure (x, s)
(u Freer.:>>= q) `run` s = case Union.decomp u of
	Left u' -> u' Freer.:>>=
		FTCQueue.singleton ((`run` s) `Freer.comp` q)
	Right Get -> q `Freer.app` s `run` s
	Right (Put s') -> q `Freer.app` () `run` s'

transaction :: forall s effs a .
	Union.Member (S s) effs => Eff.E effs a -> Eff.E effs a
transaction m = do
	(s0 :: s) <- get
	($ m) . ($ s0) $ fix \go s -> \case
		Freer.Pure x -> put s >> pure x
		u Freer.:>>= q -> case Union.prj @(S s) u of
			Nothing -> u Freer.:>>=
				FTCQueue.singleton (go s `Freer.comp` q)
			Just Get -> go s $ q `Freer.app` s
			Just (Put s') -> go s' $ q `Freer.app` ()
