{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Handle.Time (
	-- * TimeState
	TimeState(..), Mode(InitialMode),
	-- * IO Mimicable
	TaiTimeM(..), DelayM(..),
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
	getLatestTime :: s -> AbsoluteTime
	putLatestTime :: s -> AbsoluteTime -> s

instance TimeState (Mode, AbsoluteTime) where
	getMode = fst; putMode s = flip first s . const
	getLatestTime = snd; putLatestTime s = flip second s . const

updateTimeState :: TimeState s => s -> (Mode, AbsoluteTime) -> s
updateTimeState s (m, t) = s `putMode` m `putLatestTime` t

data Mode = InitialMode | FlushWaitMode AbsoluteTime deriving Show

mode :: a -> (AbsoluteTime -> a) -> Mode -> a
mode im wm = \case InitialMode -> im; FlushWaitMode t -> wm t

-- TAI TIME MONAD

class TaiTimeM m where getCurrentTime :: m AbsoluteTime
instance TaiTimeM IO where getCurrentTime = systemToTAITime <$> getSystemTime

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
handleTimeEvPlus hdl rqs i@(_, s) = mode
	(handleInit hdl rqs i) (handleWait rqs . (, s) . (, getLatestTime s))
	(getMode s)

handleInit :: (
	TimeState s, Monad m, TaiTimeM m, DelayM m,
	ExpandableHandle es (es :+: TimeEv),
	ExpandableHandle TimeEv (es :+: TimeEv),
	MergeableOccurred es TimeEv (es :+: TimeEv) ) =>
	HandleIo' ((DiffTime, a), s) s m es ->
	HandleIo' ((DiffTime, a), s) s m (es :+: TimeEv)
handleInit hdl = mergeIo
	hdl (\((d, _), s) -> s <$ delay d)
	(\rqs s -> (updateTimeState s `second`)
		<$> (handleTime rqs . (, getLatestTime s) =<< getCurrentTime))
	((<$> getCurrentTime) . putLatestTime)

handleWait :: (TimeState s, Monad m, ExpandableHandle TimeEv es) =>
	HandleIo' ((AbsoluteTime, AbsoluteTime), s) s m es
handleWait rqs (nl, s) = (updateTimeState s `second`)
	<$> expandIo handleTime (pure . (InitialMode ,) . fst) rqs nl

handleTime :: Monad m =>
	HandleIo' (AbsoluteTime, AbsoluteTime) (Mode, AbsoluteTime) m TimeEv
handleTime rqs (now, lst) = let dt = now `diffAbsoluteTime` lst in
	case project rqs of
		Just (TryWaitReq t)
			| t < dt  -> pure (
				Just $ OccTryWait t >- Singleton (OccDeltaTime t),
				(FlushWaitMode now, t `addAbsoluteTime` lst) )
			| otherwise -> pure (
				Just $ OccTryWait dt >- Singleton (OccDeltaTime dt),
				(InitialMode, now) )
		Nothing -> pure (
			Just . expand . Singleton $ OccDeltaTime dt,
			(InitialMode, now) )
