{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Boxes.Handle (SigG, handleWithoutTime, handle, AB(..)) where

import Control.Monad.State (StateT, get, put, lift)
import Data.Type.Set (Set(Nil), (:-))
import Data.UnionSet (prj, singleton, (>-), expand, collapse)
import Data.Time (DiffTime)
import Data.Time.Clock.System (getSystemTime, systemToTAITime)
import Data.Time.Clock.TAI (AbsoluteTime, diffAbsoluteTime, addAbsoluteTime)

import Trials.Boxes.Events (
	GuiEv, SigG, TryWait(..), DeltaTime(..), Occurred(..) )
	
import MonadicFrp.Handle
import MonadicFrp.Events.Mouse
import MonadicFrp.XFieldHandle.Mouse
import Field (Field)
	
handleWithoutTime :: Field -> EvReqs GuiEv -> IO (EvOccs GuiEv)
handleWithoutTime f = retry $ ((expand <$>) <$>) . maybe (pure Nothing) (handleMouse Nothing f) . collapse

data AB = A | B AbsoluteTime deriving Show

handle :: DiffTime -> Field -> HandleSt AB (StateT AbsoluteTime IO) GuiEv
handle prd f = retrySt \case A -> handleA prd f; B now -> handleB now

handleA :: DiffTime -> Field -> EvReqs GuiEv -> StateT AbsoluteTime IO (Maybe (EvOccs GuiEv), AB)
handleA = curry $ mergeHandleSt handleMouse' (const $ pure ()) handleNow
	(const $ A <$ (put . systemToTAITime =<< lift getSystemTime))

handleNow :: HandleSt' () AB (StateT AbsoluteTime IO) (TryWait :- DeltaTime :- 'Nil)
handleNow () reqs = do
	now <- lift $ systemToTAITime <$> getSystemTime
	handleTime now reqs

handleB :: Monad m => AbsoluteTime -> EvReqs GuiEv -> StateT AbsoluteTime m (Maybe (EvOccs GuiEv), AB)
handleB = expandHandleSt handleTime (\now -> A <$ put now)

handleTime :: Monad m => HandleSt' AbsoluteTime AB (StateT AbsoluteTime m) (TryWait :- DeltaTime :- 'Nil)
handleTime now reqs = do
	lst <- get
	let	dt = now `diffAbsoluteTime` lst
	case prj reqs of
		Just (TryWaitReq t)
			| t < dt -> do
				put $ t `addAbsoluteTime` lst
				pure (Just $ OccTryWait t >- singleton (OccDeltaTime t), B now)
			| otherwise -> do
				put now
				pure (Just $ OccTryWait dt >- singleton (OccDeltaTime dt), A)
		Nothing -> do
			put now
			pure (Just $ expand . singleton $ OccDeltaTime dt, A)

handleMouse' :: HandleSt' (DiffTime, Field) () (StateT AbsoluteTime IO) (MouseDown :- MouseUp :- MouseMove :- 'Nil)
handleMouse' (prd, f) reqs = do
	r1 <- lift $ handleMouse (Just prd) f reqs
	pure (r1, ())
