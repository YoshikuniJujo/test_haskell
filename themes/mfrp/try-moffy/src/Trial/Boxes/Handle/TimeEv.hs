{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, PatternSynonyms #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Boxes.Handle.TimeEv (
	TaiTimeM(..), DelayM(..), Mode(InitMode), handleTimeEvPlus ) where

import Control.Moffy.Handle hiding (expand)
import Control.Concurrent
import Data.Type.Set ((:+:))
import Data.OneOrMore (project, pattern Singleton, (>-), expand)
import Data.Time (DiffTime)
import Data.Time.Clock.System (getSystemTime, systemToTAITime)
import Data.Time.Clock.TAI (AbsoluteTime, diffAbsoluteTime, addAbsoluteTime)
	
import Trial.Boxes.Event (TimeEv, TryWait(..), pattern OccDeltaTime, pattern OccTryWait)

---------------------------------------------------------------------------

data Mode = InitMode | WaitMode AbsoluteTime deriving Show

class ModeState s where getMode :: s -> Mode; putMode :: s -> Mode -> s

instance ModeState Mode where getMode = id; putMode = flip const

handleTimeEvPlus :: (
	Monad m, TaiTimeM m, DelayM m,
	ExpandableHandle es (es :+: TimeEv),
	ExpandableHandle TimeEv (es :+: TimeEv),
	MergeableOccurred es TimeEv (es :+: TimeEv) ) =>
	(DiffTime -> a -> Handle' m es) ->
	DiffTime -> a -> HandleSt' (Mode, AbsoluteTime) (Mode, AbsoluteTime) m (es :+: TimeEv)
handleTimeEvPlus hdl prd f rqs (md, tai) = case md of
	InitMode -> handleInit' hdl rqs ((prd, f), tai)
	WaitMode now -> handleWait' rqs (now, tai)

handleInit' :: (
	Monad m, TaiTimeM m, DelayM m,
	ExpandableHandle es (es :+: TimeEv),
	ExpandableHandle TimeEv (es :+: TimeEv),
	MergeableOccurred es TimeEv (es :+: TimeEv) ) =>
	(DiffTime -> a -> Handle' m es) ->
	HandleSt' ((DiffTime, a), AbsoluteTime) (Mode, AbsoluteTime) m (es :+: TimeEv)
handleInit' hdl = mergeSt
	(toHandleSt' hdl) (\((prd, _), tai) -> tai <$ delay (round $ prd * 1000000))
	handleNow' (\_ -> (InitMode ,) <$> getTaiTime)

toHandleSt' :: Monad m => (DiffTime -> a -> Handle' m es) -> HandleSt' ((DiffTime, a), AbsoluteTime) AbsoluteTime m es
toHandleSt' hdl rqs ((prd, x), tai) = (, tai) <$> hdl prd x rqs

handleNow' :: (Monad m, TaiTimeM m) => HandleSt' AbsoluteTime (Mode, AbsoluteTime) m TimeEv
handleNow' rqs lst = handleTime' rqs . (, lst) =<< getTaiTime

class TaiTimeM m where getTaiTime :: m AbsoluteTime
class DelayM m where delay :: Int -> m ()

instance TaiTimeM IO where getTaiTime = systemToTAITime <$> getSystemTime
instance DelayM IO where delay = threadDelay

handleWait' :: (Monad m, ExpandableHandle TimeEv es) =>
	HandleSt' (AbsoluteTime, AbsoluteTime) (Mode, AbsoluteTime) m es
handleWait' = expandSt handleTime' (pure . (InitMode ,) . fst)

handleTime' :: Monad m =>
	HandleSt' (AbsoluteTime, AbsoluteTime) (Mode, AbsoluteTime) m TimeEv
handleTime' rqs (now, lst) = let -- do
	dt = now `diffAbsoluteTime` lst
	odt = Singleton $ OccDeltaTime dt in
	case project rqs of
		Just (TryWaitReq t)
			| t < dt  -> pure (
				Just $ OccTryWait t >- odt',
				(WaitMode now, t `addAbsoluteTime` lst) )
			| otherwise -> pure (Just $ OccTryWait dt >- odt', (InitMode, now))
			where odt' = Singleton $ OccDeltaTime t
		Nothing -> pure (Just . expand $ odt, (InitMode, now))
