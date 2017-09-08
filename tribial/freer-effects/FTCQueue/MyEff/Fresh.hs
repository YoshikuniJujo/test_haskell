{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MyEff.Fresh where

import MyEff

data Fresh a where Fresh :: Fresh Int

fresh :: Member Fresh effs => Eff effs Int
fresh = send Fresh

runFresh' :: Eff (Fresh ': effs) a -> Int -> Eff effs a
runFresh' m s =
	handleRelayS s (\_s a -> pure a) (\s' Fresh k -> (k $! s' + 1) s') m
