{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Run (
	-- * Type
	Sig, React, Handle, HandleSt, St,
	-- * Run
	interpret, interpretSt, interpretReact, interpretReactSt ) where

import Control.Monad.Freer.Par (pattern Pure, pattern (:=<<), app)
import Control.Moffy.Internal.Sig.Type (Sig(..), isig)
import Control.Moffy.Internal.React (Adjustable, adjust)
import Control.Moffy.Internal.React.Type (
	React, Rct(..), Handle, HandleSt, liftHandle, St,
	ThreadId, rootThreadId )

---------------------------------------------------------------------------

-- * SIG
-- * REACT

---------------------------------------------------------------------------
-- SIG
---------------------------------------------------------------------------

interpret :: (Monad m, Adjustable es es') =>
	Handle m es' -> (a -> m ()) -> Sig s es a r -> m r
interpret hdl vw rqs = fst <$> interpretSt (liftHandle hdl) vw rqs ()

interpretSt :: (Monad m, Adjustable es es') =>
	HandleSt st m es' -> (a -> m ()) -> Sig s es a r -> St st m r
interpretSt hdl vw = go where
	Sig r `go` st = interpretReactSt hdl r st >>= \(i, st') ->
		isig (pure . (, st')) ((. (`go` st')) . (>>) . vw) i

---------------------------------------------------------------------------
-- REACT
---------------------------------------------------------------------------

interpretReact :: (Monad m, Adjustable es es') =>
	Handle m es' -> React s es r -> m r
interpretReact hdl rqs = fst <$> interpretReactSt (liftHandle hdl) rqs ()

interpretReactSt :: (Monad m, Adjustable es es') =>
	HandleSt st m es' -> React s es r -> St st m r
interpretReactSt hdl (adjust -> r) = runSt hdl r rootThreadId

runSt :: Monad m => HandleSt st m es -> React s es r -> ThreadId -> St st m r
runSt _ (Pure x) _ st = pure (x, st)
runSt _ (_ :=<< Never) _ _ = error "never end"
runSt hdl (c :=<< GetThreadId) t st = runSt hdl (c `app` t) t st
runSt hdl (c :=<< Await rqs) t st =
	hdl rqs st >>= \(o, st') -> runSt hdl (c `app` o) t st'
