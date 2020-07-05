{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs, TypeFamilies, ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Internal.React.Run (interpretReact, interpretReactSt) where

import qualified Control.Arrow as A

import Control.Moffy.Internal.React.Type
import Freer

runReact :: Monad m => ThreadId -> Handle m es -> React s es a -> m (a, ThreadId)
runReact ti _ (Pure x) = pure (x, ti)
runReact _ _ (Never :>>= _) = error "never end"
runReact ti hdl (GetThreadId :>>= c) = runReact ti hdl (c `qApp` ti)
runReact ti hdl (Await rqs :>>= c) = runReact ti hdl . (c `qApp`) =<< hdl rqs

interpretReact :: Monad m => Handle m es -> React s es a -> m a
interpretReact hdl r = fst <$> runReact rootThreadId hdl r

runReactSt :: Monad m => st -> ThreadId -> HandleSt st m es -> React s es a -> m ((a, ThreadId), st)
runReactSt st ti _ (Pure x) = pure ((x, ti), st)
runReactSt _ _ _ (Never :>>= _) = error "never end"
runReactSt st ti hdl (GetThreadId :>>= c) = runReactSt st ti hdl (c `qApp` ti)
runReactSt st ti hdl (Await rqs :>>= c) = do
	(x, st') <- hdl st rqs
	runReactSt st' ti hdl (c `qApp` x)

interpretReactSt :: Monad m => st -> HandleSt st m es -> React s es a -> m (a, st)
interpretReactSt st0 hdl r = (fst `A.first`) <$> runReactSt st0 rootThreadId hdl r
