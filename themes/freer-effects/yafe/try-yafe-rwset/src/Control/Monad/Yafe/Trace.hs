{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGe LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yafe.Trace where

import Control.Monad.Yafe.Eff qualified as Eff
import Control.Monad.Freer qualified as Freer
import Control.OpenUnion qualified as Union

data Trace a where Trace :: String -> Trace ()

trace :: Union.Member Trace effs => String -> Eff.E effs ()
trace = Eff.eff . Trace

runTrace :: Eff.E '[Trace] a -> IO a
runTrace = \case
	Freer.Pure x -> pure x
	u Freer.:>>= q -> case Union.extract u of
		Trace s -> putStrLn s >> runTrace (q `Freer.app` ())
