{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MyEff.Reader (Reader, ask, runReader) where

import MyEff (Eff, Member, send, Freer(..), decomp)

data Reader e a where Reader :: Reader e e

ask :: Member (Reader e) effs => Eff effs e
ask = send Reader

runReader :: Eff (Reader e ': effs) a -> e -> Eff effs a
runReader m e = case m of
	Pure x -> Pure x
	Join u k -> case decomp u of
		Right Reader -> runReader (k e) e
		Left u' -> Join u' $ (`runReader` e) . k
