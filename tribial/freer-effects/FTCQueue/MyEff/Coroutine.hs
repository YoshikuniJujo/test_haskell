{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

module MyEff.Coroutine (Yield, Status(..), yield, runC) where

import MyEff (Eff, Member, send, handleRelay)

data Yield o i a = Yield o (i -> a)

yield :: Member (Yield o i) effs => o -> (i -> a) -> Eff effs a
yield = (send .) . Yield

data Status effs o i a
	= Done a
	| Continue o (i -> Eff effs (Status effs o i a))

runC :: Eff (Yield o i ': effs) a -> Eff effs (Status effs o i a)
runC = handleRelay (return . Done)
	$ \(Yield o k) -> return . Continue o . (. k)
