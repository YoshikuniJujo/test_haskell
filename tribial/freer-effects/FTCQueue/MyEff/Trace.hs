{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MyEff.Trace (Trace, runTrace, ignoreTrace, trace) where

import MyEff.Internal (
	Eff, Member, send,
	Freer(..), qApp, extract)

data Trace a where Trace :: String -> Trace ()

trace :: Member Trace effs => String -> Eff effs ()
trace = send . Trace

runTrace :: Eff '[Trace] a -> IO a
runTrace = \case
	Pure x -> return x
	Join u q -> case extract u of
		Trace s -> putStrLn s >> runTrace (q `qApp` ())

ignoreTrace :: Eff '[Trace] a -> a
ignoreTrace = \case
	Pure x -> x
	Join u q -> case extract u of Trace _ -> ignoreTrace $ q `qApp` ()
