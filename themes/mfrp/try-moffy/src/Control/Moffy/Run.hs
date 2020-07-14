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
	(x, st') <- hdl rqs st
	runReactSt st' ti hdl (c `qApp` x)

interpretReactSt :: Monad m => st -> HandleSt st m es -> React s es a -> m (a, st)
interpretReactSt st0 hdl r = (fst `first`) <$> runReactSt st0 rootThreadId hdl r

interpret :: Monad m => Handle m es -> (a -> m ()) -> Sig s es a r -> m r
interpret hdl vw = go where
	go (Sig s) = interpretReact hdl s >>= isig pure \h -> (vw h >>) . go

interpretSt ::
	Monad m => st -> HandleSt st m es -> (a -> m ()) -> Sig s es a r -> m r
interpretSt st0 hdl vw = go st0 where
	go st (Sig s) = interpretReactSt st hdl s >>= \(is, st') ->
		isig pure (\h -> (vw h >>) . go st') is
