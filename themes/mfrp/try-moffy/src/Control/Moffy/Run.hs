{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Run (
	-- * Type
	Sig, React, Handle, HandleSt,
	-- * Run
	interpret, interpretSt, interpretReact, interpretReactSt ) where

import Control.Arrow (first)
import Control.Monad.Freer.Par (Freer(..), pattern (:>>=), qApp)
import Control.Moffy.Internal.Sig.Type (Sig(..), isig)
import Control.Moffy.Internal.React.Type (
	Handle, HandleSt, React, Rct(..), ThreadId, rootThreadId )

---------------------------------------------------------------------------

-- * RUN SIG
-- * RUN REACT

---------------------------------------------------------------------------
-- RUN SIG
---------------------------------------------------------------------------

interpret :: Monad m => Handle m es -> (a -> m ()) -> Sig s es a r -> m r
interpret hdl vw = go where
	go (Sig s) = isig pure ((. go) . (>>) . vw) =<< interpretReact hdl s

interpretSt ::
	Monad m => HandleSt st m es -> st -> (a -> m ()) -> Sig s es a r -> m r
interpretSt hdl st0 vw = (`go` st0) where
	Sig s `go` st = interpretReactSt hdl st s >>= \(is, st') ->
		isig pure ((. (`go` st')) . (>>) . vw) is

---------------------------------------------------------------------------
-- RUN REACT
---------------------------------------------------------------------------

interpretReact :: Monad m => Handle m es -> React s es a -> m a
interpretReact hdl r = fst <$> runReact hdl r rootThreadId

runReact ::
	Monad m => Handle m es -> React s es a -> ThreadId -> m (a, ThreadId)
runReact _ (Pure x) t = pure (x, t)
runReact _ (Never :>>= _) _ = error "never end"
runReact h (GetThreadId :>>= c) t = runReact h (c `qApp` t) t
runReact h (Await rqs :>>= c) t = flip (runReact h) t . (c `qApp`) =<< h rqs

interpretReactSt ::
	Monad m => HandleSt st m es -> st -> React s es a -> m (a, st)
interpretReactSt hdl st r = (fst `first`) <$> runReactSt hdl st r rootThreadId

runReactSt :: Monad m =>
	HandleSt st m es -> st -> React s es a -> ThreadId ->
	m ((a, ThreadId), st)
runReactSt _ st (Pure x) t = pure ((x, t), st)
runReactSt _ _ (Never :>>= _) _ = error "never end"
runReactSt h st (GetThreadId :>>= c) t = runReactSt h st (c `qApp` t) t
runReactSt h st (Await rqs :>>= c) t =
	h rqs st >>= \(x, st') -> runReactSt h st' (c `qApp` x) t
