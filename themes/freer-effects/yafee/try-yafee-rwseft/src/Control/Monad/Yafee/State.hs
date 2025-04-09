{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yafee.State (

	-- * NORMAL

	S, get, gets, put, modify, run, transaction,

	-- * NAMED

	Named(..), getN, putN, modifyN, runN, transactionN

	) where

import GHC.TypeLits
import Control.Monad.Fix
import Control.Monad.Yafee.Eff qualified as Eff
import Control.Monad.Freer qualified as Freer
import Control.OpenUnion qualified as Union
import Data.FTCQueue qualified as FTCQueue

-- NORMAL

type S = Named ""

data Named (nm :: Symbol) s a where Get :: Named nm s s; Put :: forall nm s . !s -> Named nm s ()

get :: Union.Member (S s) effs => Eff.E effs s
get = getN @""

gets :: Union.Member (S s) effs => (s -> t) -> Eff.E effs t
gets f = f <$> get

put :: Union.Member (S s) effs => s -> Eff.E effs ()
put = putN @""

modify :: Union.Member (S s) effs => (s -> s) -> Eff.E effs ()
modify = modifyN @""

run :: Eff.E (S s ': effs) a -> s -> Eff.E effs (a, s)
run = runN @""

transaction :: forall s effs a .
	Union.Member (S s) effs => Eff.E effs a -> Eff.E effs a
transaction = transactionN @"" @s

-- NAMED

getN :: forall nm s effs . Union.Member (Named nm s) effs => Eff.E effs s
getN = Eff.eff (Get @nm)

putN :: forall nm s effs . Union.Member (Named nm s) effs => s -> Eff.E effs ()
putN = Eff.eff . (Put @nm)

modifyN :: forall nm s effs . Union.Member (Named nm s) effs => (s -> s) -> Eff.E effs ()
modifyN f = putN @nm . f =<< getN @nm

runN :: Eff.E (Named nm s ': effs) a -> s -> Eff.E effs (a, s)
runN = Eff.handleRelayS
	(curry pure) \u k s -> case u of Get -> k s s; Put s' -> k () s'

transactionN :: forall nm s effs a .
	Union.Member (Named nm s) effs => Eff.E effs a -> Eff.E effs a
transactionN m = do
	(s0 :: s) <- getN @nm
	($ m) . ($ s0) $ fix \go s -> \case
		Freer.Pure x -> putN @nm s >> pure x
		u Freer.:>>= q -> case Union.prj @(Named nm s) u of
			Nothing -> u Freer.:>>=
				FTCQueue.singleton (go s `Freer.comp` q)
			Just Get -> go s $ q `Freer.app` s
			Just (Put s') -> go s' $ q `Freer.app` ()
