{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MyEff.Fresh (Fresh, runFresh', fresh)  where

import MyEff (Eff, Member, send, handleRelayS)

data Fresh a where Fresh :: Fresh Int

fresh :: Member Fresh effs => Eff effs Int
fresh = send Fresh

runFresh' :: Eff (Fresh ': effs) a -> Int -> Eff effs a
runFresh' m s0 =
	handleRelayS s0 (const pure) (\s Fresh f -> (f $! s + 1) s) m
