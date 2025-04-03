{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module UseFTCQueue.BasicMonads where

import Control.Arrow (second)
import Control.Monad.Fix
import UseFTCQueue.Eff
import UseFTCQueue.Freer
import OpenUnion

data Reader e a where Ask :: Reader e e

ask :: Member (Reader e) effs => Eff effs e
ask = eff Ask

asks :: Member (Reader e) effs => (e -> a) -> Eff effs a
asks f = f <$> ask

local :: forall e effs a .
	Member (Reader e) effs => (e -> e) -> Eff effs a -> Eff effs a
local f m = do
	e <- f <$> ask
	let	h :: Reader e v -> (v -> Eff effs a) -> Eff effs a
		h Ask k = k e
	interpose pure h m

runReader :: Eff (Reader e ': effs) a -> e -> Eff effs a
m `runReader` e = handleRelay pure (\Ask k -> k e) m

data Writer w a where Tell :: w -> Writer w ()

tell :: Member (Writer w) effs => w -> Eff effs ()
tell = eff . Tell

runWriter :: Monoid w => Eff (Writer w ': effs) a -> Eff effs (a, w)
runWriter = handleRelay
	(pure . (, mempty)) \(Tell w) -> (((w <>) `second`) <$>) . ($ ())

data State s a where Get :: State s s; Put :: !s -> State s ()

get :: Member (State s) effs => Eff effs s
get = eff Get

put :: Member (State s) effs => s -> Eff effs ()
put = eff . Put

modify :: Member (State s) effs => (s -> s) -> Eff effs ()
modify f = put . f =<< get

runState :: Eff (State s ': effs) a -> s -> Eff effs (a, s)
Pure x `runState` s = pure (x, s)
(u `Bind` q) `runState` s = case decomp u of
	Left u' -> u' `Bind` singleton ((`runState` s) `comp` q)
	Right Get -> q `app` s `runState` s
	Right (Put s') ->q `app` () `runState` s'

transactionState ::
	forall s effs a . Member (State s) effs => Eff effs a -> Eff effs a
transactionState m = do
	(s0 :: s) <- get
	($ m) . ($ s0) $ fix \go s -> \case
		Pure x -> put s >> pure x
		u `Bind` q -> case prj @(State s) u of
			Nothing -> u `Bind` singleton (go s `comp` q)
			Just Get -> go s $ q `app` s
			Just (Put s') -> go s' $ q `app` ()

data Exc e a = ThrowError e deriving Show

throwError :: Member (Exc e) effs => e -> Eff effs a
throwError = eff . ThrowError

runError :: Eff (Exc e ': effs) a -> Eff effs (Either e a)
runError = handleRelay (pure . Right) (\(ThrowError e) -> const . pure $ Left e)

catchError :: Member (Exc e) effs => Eff effs a -> (e -> Eff effs a) -> Eff effs a
m `catchError` h = interpose pure (\(ThrowError e) -> const $ h e) m

data Trace a where Trace :: String -> Trace ()

trace :: Member Trace effs => String -> Eff effs ()
trace = eff . Trace

runTrace :: Eff '[Trace] a -> IO a
runTrace (Pure x) = pure x
runTrace (u `Bind` q) = case extract u of
	Trace s -> putStrLn s >> runTrace (q `app` ())
