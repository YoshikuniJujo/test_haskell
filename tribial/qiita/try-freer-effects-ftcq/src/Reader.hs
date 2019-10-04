{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Reader (Reader(..), runReader, ask) where

import Eff (Eff, Freer(..), Member, inj, decomp, tsingleton, qApp, qComp)

data Reader e a where Reader :: Reader e e

ask :: Member (Reader e) effs => Eff effs e
ask = inj Reader `Bind` tsingleton Pure

runReader :: Eff (Reader e ': effs) a -> e -> Eff effs a
runReader m e = case m of
	Pure x -> Pure x
	u `Bind` k -> case decomp u of
		Right Reader -> runReader (k `qApp` e) e
		Left u' -> u' `Bind` tsingleton (k `qComp` (`runReader` e))
