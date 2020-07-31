{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, PatternSynonyms #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Boxes.Handle.TimeEv (Mode(InitMode), handleTimeEvPlus) where

import Control.Concurrent
import Control.Moffy.Handle hiding (expand)
import Control.Monad.State (StateT, get, put, lift, liftIO)
import Data.Type.Set ((:+:))
import Data.OneOrMore (project, pattern Singleton, (>-), expand)
import Data.Time (DiffTime)
import Data.Time.Clock.System (getSystemTime, systemToTAITime)
import Data.Time.Clock.TAI (AbsoluteTime, diffAbsoluteTime, addAbsoluteTime)
	
import Trial.Boxes.Event (TimeEv, TryWait(..), pattern OccDeltaTime, pattern OccTryWait)

---------------------------------------------------------------------------

data Mode = InitMode | WaitMode AbsoluteTime deriving Show

handleTimeEvPlus :: (
	ExpandableHandle es (es :+: TimeEv),
	ExpandableHandle TimeEv (es :+: TimeEv),
	MergeableOccurred es TimeEv (es :+: TimeEv) ) =>
	(DiffTime -> a -> Handle' IO es) ->
	DiffTime -> a -> HandleSt Mode (StateT AbsoluteTime IO) (es :+: TimeEv)
handleTimeEvPlus hdl prd f rqs = (`retrySt` rqs) \rqs' -> \case
	InitMode -> (handleInit hdl rqs' (prd, f)); WaitMode now -> (rqs' `handleWait` now)

handleInit :: (
	ExpandableHandle es (es :+: TimeEv),
	ExpandableHandle TimeEv (es :+: TimeEv),
	MergeableOccurred es TimeEv (es :+: TimeEv) ) =>
	(DiffTime -> a -> Handle' IO es) ->
	HandleSt' (DiffTime, a) Mode (StateT AbsoluteTime IO) (es :+: TimeEv)
handleInit hdl = mergeSt
	(toHandleSt' hdl) (\(prd, _) -> liftIO . threadDelay . round $ prd * 1000000)
	handleNow (\_ -> InitMode <$ (put =<< lift getTaiTime))

toHandleSt' :: Monad m => (DiffTime -> a -> Handle' m es) -> HandleSt' (DiffTime, a) () (StateT AbsoluteTime m) es
toHandleSt' hdl reqs (prd, x) = lift $ (, ()) <$> hdl prd x reqs

handleNow :: HandleSt' () Mode (StateT AbsoluteTime IO) TimeEv
handleNow reqs () = (reqs `handleTime`) =<< lift getTaiTime

getTaiTime :: IO AbsoluteTime
getTaiTime = systemToTAITime <$> getSystemTime

handleWait :: (Monad m, ExpandableHandle TimeEv es) =>
	HandleSt' AbsoluteTime Mode (StateT AbsoluteTime m) es
handleWait = expandSt handleTime ((InitMode <$) . put)

handleTime :: Monad m =>
	HandleSt' AbsoluteTime Mode (StateT AbsoluteTime m) TimeEv
handleTime reqs now = get >>= \lst -> do
	let	dt = now `diffAbsoluteTime` lst
		odt = Singleton $ OccDeltaTime dt
	case project reqs of
		Just (TryWaitReq t)
			| t < dt -> (Just $ OccTryWait t >- odt', WaitMode now)
				<$ put (t `addAbsoluteTime` lst)
			| otherwise -> (Just $ OccTryWait dt >- odt, InitMode)
				<$ put now
			where odt' = Singleton $ OccDeltaTime t
		Nothing -> (Just . expand $ odt, InitMode) <$ put now
