{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.HigherOpenUnion qualified as U

someFunc :: IO ()
someFunc = putStrLn "someFunc"

try :: forall e s i o r . Num s => Eff.E '[Except.E e, State.S s] i o r -> (Either e r, s)
try = Eff.run . (`State.run` 0) . Except.run

try' :: forall e s i o r . Num s => Eff.E '[State.S s, Except.E e] i o r -> Either e (r, s)
try' = Eff.run . Except.run . (`State.run` 0)

transact :: (
	U.Member (State.S Integer) es,
	U.Member (Except.E String) es
	) =>
	Eff.E es i o Integer
transact = do
	State.put 1
	do { State.put 2; Except.throw "ERROR" } `Except.catch` \(e :: String) -> pure e
	State.get
