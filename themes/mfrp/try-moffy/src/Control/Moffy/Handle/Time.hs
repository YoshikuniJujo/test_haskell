{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Handle.Time (
	-- * Class
	TimeState(..), TaiTimeM(..), DelayM(..),
	-- * Mode
	Mode(InitialMode),
	-- * Handle
	handleTimeEvPlus ) where

import Control.Arrow (first, second)
import Control.Moffy.Event.Time (
	TimeEv, pattern OccDeltaTime, TryWait(..), pattern OccTryWait )
import Control.Moffy.Handle (
	ExpandableHandle, MergeableOccurred, HandleIo', expandIo, mergeIo )
import Control.Concurrent (threadDelay)
import Data.Type.Set ((:+:))
import Data.OneOrMore (pattern Singleton, (>-), project, expand)
import Data.Time (DiffTime)
import Data.Time.Clock.TAI (AbsoluteTime, diffAbsoluteTime, addAbsoluteTime)
import Data.Time.Clock.System (getSystemTime, systemToTAITime)
	
---------------------------------------------------------------------------

-- * CLASS
--	+ TIME STATE
--	+ TAI TIME MONAD
--	+ DELAY MONAD
-- * HANDLE

---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- CLASS
---------------------------------------------------------------------------

-- TIME STATE

class TimeState s where
	getMode :: s -> Mode; putMode :: s -> Mode -> s
	getTai :: s -> AbsoluteTime; putTai :: s -> AbsoluteTime -> s

data Mode = InitialMode | WaitMode AbsoluteTime deriving Show

instance TimeState (Mode, AbsoluteTime) where
	getMode = fst; putMode s = flip first s . const
	getTai = snd; putTai s = flip second s . const

updateTimeState :: TimeState s => s -> (Mode, AbsoluteTime) -> s
updateTimeState s (m, t) = s `putMode` m `putTai` t

-- TAI TIME MONAD

class TaiTimeM m where getTaiTime :: m AbsoluteTime
instance TaiTimeM IO where getTaiTime = systemToTAITime <$> getSystemTime

-- DELAY MONAD

class DelayM m where delay :: DiffTime -> m ()
instance DelayM IO where delay = threadDelay . round . (* 10 ^ (6 :: Int))

---------------------------------------------------------------------------
-- HANDLE
---------------------------------------------------------------------------

handleTimeEvPlus :: (
	TimeState s, Monad m, TaiTimeM m, DelayM m,
	ExpandableHandle es (es :+: TimeEv),
	ExpandableHandle TimeEv (es :+: TimeEv),
	MergeableOccurred es TimeEv (es :+: TimeEv) ) =>
	HandleIo' ((DiffTime, a), s) s m es ->
	HandleIo' ((DiffTime, a), s) s m (es :+: TimeEv)
handleTimeEvPlus hdl rqs (prdf, s) = case getMode s of
	InitialMode -> handleInit hdl rqs (prdf, s)
	WaitMode now -> handleWait rqs ((now, getTai s), s)

handleInit :: (
	TimeState s, Monad m, TaiTimeM m, DelayM m,
	ExpandableHandle es (es :+: TimeEv),
	ExpandableHandle TimeEv (es :+: TimeEv),
	MergeableOccurred es TimeEv (es :+: TimeEv) ) =>
	HandleIo' ((DiffTime, a), s) s m es ->
	HandleIo' ((DiffTime, a), s) s m (es :+: TimeEv)
handleInit hdl = mergeIo
	hdl (\((prd, _), s) -> s <$ delay prd)
	handleNow' (\s  -> putTai s <$> getTaiTime)
	where handleNow' rqs s = second (updateTimeState s) <$> (handleTime rqs . (, getTai s) =<< getTaiTime)

handleWait :: (TimeState s, Monad m, ExpandableHandle TimeEv es) =>
	HandleIo' ((AbsoluteTime, AbsoluteTime), s) s m es
handleWait rqs ((now, lst), s) = second (updateTimeState s) <$> expandIo handleTime (pure . (InitialMode ,) . fst) rqs (now, lst)

handleTime :: Monad m =>
	HandleIo' (AbsoluteTime, AbsoluteTime) (Mode, AbsoluteTime) m TimeEv
handleTime rqs (now, lst) = let
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
