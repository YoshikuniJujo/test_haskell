{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import MyEff
import MyEff.State
import MyEff.Exception

sample :: (Member (State Integer) effs, Member (Exc String) effs) =>
	Eff effs Integer
sample = do
	n <- get
	throwError "hello"
	return n

test1 :: Either String (Integer, Integer)
test1 = run . runError $ sample `runState` 123

test2 :: (Either String Integer, Integer)
test2 = run $ runError sample `runState` 123
