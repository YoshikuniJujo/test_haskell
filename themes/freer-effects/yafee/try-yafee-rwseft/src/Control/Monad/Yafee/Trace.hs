{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGe LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yafee.Trace (T(..), trace, run) where

import Control.Monad.Yafee.Eff qualified as Eff
import Control.Monad.Freer qualified as Freer
import Control.OpenUnion qualified as Union

data T a where T :: String -> T ()

trace :: Union.Member T effs => String -> Eff.E effs ()
trace = Eff.eff . T

run :: Eff.E '[T] a -> IO a
run = \case
	Freer.Pure x -> pure x
	u Freer.:>>= q -> case Union.extract u of
		T s -> putStrLn s >> run (q `Freer.app` ())
