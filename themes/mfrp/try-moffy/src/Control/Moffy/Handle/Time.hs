{-# LANGUAGE LambdaCase, TupleSections #-}
{-# LANGUAGE PatternSynonyms, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Handle.Time (
	-- * TimeState
	TimeState(..), Mode(InitialMode),
	-- * IO Mimicable
	TaiTimeM(..), DelayM(..),
	-- * Handle
	Timable, handleTimeEvPlus ) where

import Control.Arrow (first, second, (>>>))
import Control.Moffy.Event.Time (
	TimeEv, pattern OccDeltaTime, TryWait(..), pattern OccTryWait )
import Control.Moffy.Handle (
	ExpandableHandle, MergeableOccurred, HandleIo', expandIo, mergeIo )
import Control.Concurrent (threadDelay)
import Data.Type.Set ((:+:))
import Data.OneOrMore (project)
import Data.Time (DiffTime)
import Data.Time.Clock.TAI (AbsoluteTime, diffAbsoluteTime, addAbsoluteTime)
import Data.Time.Clock.System (getSystemTime, systemToTAITime)

import Data.OneOrMoreApp (expand, pattern Singleton, (>-))
	
---------------------------------------------------------------------------

-- * TIME STATE
-- * IO MIMICABLE
-- * HANDLE

---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- TIME STATE
---------------------------------------------------------------------------

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
mode im fwm = \case InitialMode -> im; FlushWaitMode t -> fwm t

---------------------------------------------------------------------------
-- IO MIMICABLE
---------------------------------------------------------------------------

class TaiTimeM m where getCurrentTime :: m AbsoluteTime
instance TaiTimeM IO where getCurrentTime = systemToTAITime <$> getSystemTime

class DelayM m where delay :: DiffTime -> m ()
instance DelayM IO where delay = threadDelay . round . (* 10 ^ (6 :: Int))

---------------------------------------------------------------------------
-- HANDLE
---------------------------------------------------------------------------

type Timable es = (
	ExpandableHandle es (es :+: TimeEv),
	ExpandableHandle TimeEv (es :+: TimeEv),
	MergeableOccurred es TimeEv (es :+: TimeEv) )

handleTimeEvPlus :: (TimeState s, Monad m, TaiTimeM m, DelayM m, Timable es) =>
	HandleIo' ((DiffTime, a), s) s m es ->
	HandleIo' ((DiffTime, a), s) s m (es :+: TimeEv)
handleTimeEvPlus hdl rqs i@(_, s) = ($ s) $ getMode
	>>> mode (handleI hdl rqs i) (handleF rqs . (, s) . (, getLatestTime s))

handleI :: (TimeState s, Monad m, TaiTimeM m, DelayM m, Timable es) =>
	HandleIo' ((DiffTime, a), s) s m es ->
	HandleIo' ((DiffTime, a), s) s m (es :+: TimeEv)
handleI hdl = mergeIo
	hdl (\((d, _), s) -> s <$ delay d)
	(\rqs s -> (updateTimeState s `second`)
		<$> (handleTime rqs . (, getLatestTime s) =<< getCurrentTime))
	((<$> getCurrentTime) . putLatestTime)

handleF :: (TimeState s, Monad m, ExpandableHandle TimeEv es) =>
	HandleIo' ((AbsoluteTime, AbsoluteTime), s) s m es
handleF rqs (nl, s) = (updateTimeState s `second`)
	<$> expandIo handleTime (pure . (InitialMode ,) . fst) rqs nl

handleTime :: Monad m =>
	HandleIo' (AbsoluteTime, AbsoluteTime) (Mode, AbsoluteTime) m TimeEv
handleTime rqs (now, lst) = case project rqs of
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
	where dt = now `diffAbsoluteTime` lst
