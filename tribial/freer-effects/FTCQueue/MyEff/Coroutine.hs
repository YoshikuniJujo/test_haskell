{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds, TypeOperators #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

module MyEff.Coroutine (Yield, Status(..), runC, yield) where

import MyEff

data Yield a b c = Yield a (b -> c)

yield :: Member (Yield a b) effs => a -> (b -> c) -> Eff effs c
yield = (send .) . Yield

data Status effs a b c
	= Done c
	| Continue a (b -> Eff effs (Status effs a b c))

runC :: Eff (Yield a b ': effs) w -> Eff effs (Status effs a b w)
runC = handleRelay (return . Done) handler
	where
	handler :: Yield a b c ->
		(c -> Eff effs (Status effs a b d)) ->
		Eff effs (Status effs a b d)
	handler (Yield a k) arr = return . Continue a $ arr . k
