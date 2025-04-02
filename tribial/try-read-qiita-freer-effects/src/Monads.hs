{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Monads where

import Control.Arrow
import Eff (Eff, Freer(..), Member, inj, prj, decomp)

data Reader e a where Ask :: Reader e e

ask :: Member (Reader e) effs => Eff effs e
ask = inj Ask `Bind` Pure

runReader :: Eff (Reader e ': effs) a -> e -> Eff effs a
m `runReader` e = case m of
	Pure x -> Pure x
	u `Bind` k -> case decomp u of
		Right Ask -> k e `runReader` e
		Left u' -> u' `Bind` ((`runReader` e) . k)

data Writer w a where Tell :: w -> Writer w ()

tell :: Member (Writer w) effs => w -> Eff effs ()
tell = (`Bind` Pure) . inj . Tell

runWriter :: Monoid w => Eff (Writer w ': effs) a -> Eff effs (a, w)
runWriter = \case
	Pure x -> Pure (x, mempty)
	u `Bind` k -> case decomp u of
		Right (Tell w) -> second (w <>) <$> runWriter (k ())
		Left u' -> u' `Bind` (runWriter . k)

data State s a where Get :: State s s; Put :: s -> State s ()

get :: Member (State s) effs => Eff effs s
get = inj Get `Bind` Pure

put :: Member (State s) effs => s -> Eff effs ()
put = (`Bind` Pure) . inj . Put

runState :: Eff (State s ': effs) a -> s -> Eff effs (a, s)
m `runState` s0 = case m of
	Pure x -> Pure (x, s0)
	u `Bind` k -> case decomp u of
		Right Get -> k s0 `runState` s0
		Right (Put s) -> k () `runState` s
		Left u' -> u' `Bind` ((`runState` s0) . k)

modify :: Member (State s) effs => (s -> s) -> Eff effs ()
modify f = put . f =<< get

newtype Exc e a = ThrowError e

throwError :: Member (Exc e) effs => e -> Eff effs a
throwError = (`Bind` Pure) . inj . ThrowError

runError :: Eff (Exc e ': effs) a -> Eff effs (Either e a)
runError = \case
	Pure x -> Pure $ Right x
	u `Bind` k -> case decomp u of
		Right (ThrowError e) -> Pure $ Left e
		Left u' -> u' `Bind` (runError . k)

catchError ::
	Member (Exc e) effs => Eff effs a -> (e -> Eff effs a) -> Eff effs a
m `catchError` h = case m of
	Pure x -> Pure x
	u `Bind` k -> case prj u of
		Just (ThrowError e) -> h e
		Nothing -> u `Bind` ((`catchError` h) . k)
