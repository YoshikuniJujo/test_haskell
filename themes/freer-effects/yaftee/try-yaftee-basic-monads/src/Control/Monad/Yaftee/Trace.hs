{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Trace (T, trace, run) where

import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.HigherFreer qualified as HFreer
import Control.HigherOpenUnion qualified as Union

type T = Union.FromFirst T_
data T_ a where T_ :: String -> T_ ()

trace :: Union.Base T effs => String -> Eff.E effs i o ()
trace = Eff.effBase . T_

run :: Eff.E '[T] i o a -> IO a
run = \case
	HFreer.Pure x -> pure x
	u HFreer.:>>= q -> case Union.extracth u of
		Union.FromFirst (T_ s) k -> putStrLn s >> run (q HFreer.$ k ())
