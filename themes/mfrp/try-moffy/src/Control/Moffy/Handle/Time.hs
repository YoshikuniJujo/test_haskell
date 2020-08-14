{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, PatternSynonyms #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Handle.Time (
	-- * Class
	TimeState(..), TaiTimeM(..), DelayM(..),
	-- * Mode
	Mode(InitialMode),
	-- * Handle
	handleTimeEvPlus ) where

import Control.Arrow
import Control.Moffy.Handle hiding (expand)
import Control.Concurrent
import Data.Type.Set ((:+:))
import Data.OneOrMore (project, pattern Singleton, (>-), expand)
import Data.Time (DiffTime)
import Data.Time.Clock.System (getSystemTime, systemToTAITime)
import Data.Time.Clock.TAI (AbsoluteTime, diffAbsoluteTime, addAbsoluteTime)
	
import Control.Moffy.Event.Time (TimeEv, TryWait(..), pattern OccDeltaTime, pattern OccTryWait)

---------------------------------------------------------------------------

data Mode = InitialMode | WaitMode AbsoluteTime deriving Show

class TimeState s where
	getMode :: s -> Mode; putMode :: s -> Mode -> s
	getTai :: s -> AbsoluteTime; putTai :: s -> AbsoluteTime -> s

instance TimeState (Mode, AbsoluteTime) where
	getMode = fst; putMode s = flip first s . const
	getTai = snd; putTai s = flip second s . const

handleTimeEvPlus :: (
	TimeState s,
	Monad m, TaiTimeM m, DelayM m,
	ExpandableHandle es (es :+: TimeEv),
	ExpandableHandle TimeEv (es :+: TimeEv),
	MergeableOccurred es TimeEv (es :+: TimeEv) ) =>
	HandleIo' ((DiffTime, a), s) s m es ->
	HandleIo' ((DiffTime, a), s) s m (es :+: TimeEv)
handleTimeEvPlus hdl rqs0 ((prd, f0), s0) = case md of
	InitialMode -> handleInit hdl rqs0 ((prd, f0), s0)
	WaitMode now -> pt <$> handleWait rqs0 (now, tai)
	where
	md = getMode s0; tai = getTai s0
	pt (r, (m, t)) = (r, (`putMode` m) $ (`putTai` t) s0)

handleInit :: (
	TimeState s,
	Monad m, TaiTimeM m, DelayM m,
	ExpandableHandle es (es :+: TimeEv),
	ExpandableHandle TimeEv (es :+: TimeEv),
	MergeableOccurred es TimeEv (es :+: TimeEv) ) =>
	HandleIo' ((DiffTime, a), s) s m es ->
	HandleIo' ((DiffTime, a), s) s m (es :+: TimeEv)
handleInit hdl = mergeIo
	hdl (\((prd, _), s) -> s <$ delay prd)
	handleNow' (\s  -> putTai s <$> getTaiTime)

handleNow' :: (Monad m, TaiTimeM m, TimeState s) => HandleIo' s s m TimeEv
handleNow' rqs s = do
	(r, (md, tai)) <- handleNow rqs $ getTai s
	pure (r, (`putMode` md) $ (`putTai` tai) s)

handleNow :: (Monad m, TaiTimeM m) => HandleIo' AbsoluteTime (Mode, AbsoluteTime) m TimeEv
handleNow rqs lst = handleTime rqs . (, lst) =<< getTaiTime

class TaiTimeM m where getTaiTime :: m AbsoluteTime
class DelayM m where delay :: DiffTime -> m ()

instance TaiTimeM IO where getTaiTime = systemToTAITime <$> getSystemTime
instance DelayM IO where delay = threadDelay . round . (* 10 ^ (6 :: Int))

handleWait :: (Monad m, ExpandableHandle TimeEv es) =>
	HandleIo' (AbsoluteTime, AbsoluteTime) (Mode, AbsoluteTime) m es
handleWait = expandIo handleTime (pure . (InitialMode ,) . fst)

handleTime :: Monad m =>
	HandleIo' (AbsoluteTime, AbsoluteTime) (Mode, AbsoluteTime) m TimeEv
handleTime rqs (now, lst) = let -- do
	dt = now `diffAbsoluteTime` lst
	odt = Singleton $ OccDeltaTime dt in
	case project rqs of
		Just (TryWaitReq t)
			| t < dt  -> pure (
				Just $ OccTryWait t >- odt',
				(WaitMode now, t `addAbsoluteTime` lst) )
			| otherwise -> pure (Just $ OccTryWait dt >- odt', (InitialMode, now))
			where odt' = Singleton $ OccDeltaTime t
		Nothing -> pure (Just . expand $ odt, (InitialMode, now))
