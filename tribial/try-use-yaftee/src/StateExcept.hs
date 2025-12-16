{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module StateExcept where

import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.HigherOpenUnion qualified as U

someFunc :: IO ()
someFunc = putStrLn "someFunc"

try :: Eff.E '[Except.E String, State.S Int] i o r -> (Either String r, Int)
try = Eff.run . (`State.run` 0) . Except.run

try' :: Eff.E '[State.S Int, Except.E String] i o r -> Either String (r, Int)
try' = Eff.run . Except.run . (`State.run` 0)

transact :: (
	U.Member (State.S Int) es,
	U.Member (Except.E String) es
	) =>
	Eff.E es i o Int
transact = do
	State.put (1 :: Int)
	_ <- do { State.put (2 :: Int); Except.throw "ERROR" }
			`Except.catch` \(e :: String) -> pure e
	State.get
