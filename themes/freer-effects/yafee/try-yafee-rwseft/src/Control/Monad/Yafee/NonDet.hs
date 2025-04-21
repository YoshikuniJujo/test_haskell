{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yafee.NonDet where

import Control.Applicative
import Control.Monad
import Control.Monad.Freer qualified as Freer
import Control.Monad.Yafee.Eff qualified as Eff
import Control.OpenUnion qualified as Union
import Data.FTCQueue qualified as FTCQueue

type N = Union.NonDet

run :: Alternative f =>
	Eff.E (N ': effs) a -> Eff.E effs (f a)
run = Eff.handleRelay (pure . pure) $ \m k -> case m of
	Union.MZero -> pure empty
	Union.MPlus -> liftA2 (<|>) (k True) (k False)

split :: Union.Member N effs =>
	Eff.E effs a -> Eff.E effs (Maybe (a, Eff.E effs a))
split = go []
	where
	go jq (Freer.Pure x) = pure (Just (x, msum jq))
	go jq (u Freer.:>>= q) = case Union.prj u of
		Just Union.MZero -> case jq of
			[] -> pure Nothing
			j : jq' -> go jq' j
		Just Union.MPlus -> go (q `Freer.app` False : jq) (q `Freer.app` True)
		Nothing -> u Freer.:>>= FTCQueue.singleton (go jq `Freer.comp` q)
