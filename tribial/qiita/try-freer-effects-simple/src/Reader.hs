{-# LANGUAGE GADTs, DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Reader (Reader, ask, runReader) where

import Eff (Eff, Freer(..), Member, inj, decomp)

data Reader e a where Reader :: Reader e e

ask :: Member (Reader e) effs => Eff effs e
ask = inj Reader `Bind` Pure

runReader :: Eff (Reader e ': effs) a -> e -> Eff effs a
runReader m e = case m of
	Pure x -> Pure x
	u `Bind` k -> case decomp u of
		Right Reader -> runReader (k e) e
		Left u' -> u' `Bind` ((`runReader` e) . k)
