{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Sleep where

import Control.Concurrent
import Data.Type.Flip
import Data.Type.Set
import Data.UnionSet
import Data.Bool
import Data.Time

import MonadicFrp
import MonadicFrp.Run
import MonadicFrp.Handle

data TryWait = TryWaitReq { getTryWaitReq :: DiffTime } deriving (Show, Eq, Ord)
numbered 8 [t| TryWait |]
instance Request TryWait where
	data Occurred TryWait = OccTryWait DiffTime deriving Show

tryWait :: DiffTime -> React (Singleton TryWait) DiffTime
tryWait t = await (TryWaitReq t) \(OccTryWait t') -> t'

sleep :: DiffTime -> React (Singleton TryWait) ()
sleep t = tryWait t >>= \t' -> bool (sleep (t - t')) (pure ()) (t' == t)
-- sleep t = tryWait t >>= \t' -> bool (sleep (t - t')) (pure ()) (t' >= t)

data Tick = Tick Int deriving Show

data Tack = Tack Int deriving Show

tick :: Int -> Sig (Singleton TryWait) Tick ()
tick n = emit (Tick n) >> waitFor (sleep 3) >> tick (n + 1)

tack :: Int -> Sig (Singleton TryWait) Tack ()
tack n = emit (Tack n) >> waitFor (sleep 5) >> tack (n + 1)

tickTack :: Sig (Singleton TryWait) (Tick, Tack) ()
tickTack = (,) <$%> tick 0 <*%> tack 0

handle :: Handle IO (Singleton TryWait)
handle _reqs = do
	threadDelay 500000
	pure . singleton $ OccTryWait 0.5
--	where TryWaitReq t = extract reqs

runSleep :: IO ()
runSleep = interpret handle print tickTack

data AB = A | B DiffTime deriving Show

handle' :: HandleSt AB IO (Singleton TryWait)
handle' = \case A -> handleA; B t -> handleB t

handleA :: EvReqs (Singleton TryWait) -> IO (EvOccs (Singleton TryWait), AB)
handleA reqs = do
	threadDelay 2000000
	bool	(pure (singleton $ OccTryWait 2, A))
		(pure (singleton $ OccTryWait t, B $ 2 - t))
		(t < 2)
	where TryWaitReq t = extract reqs

handleB :: Applicative m =>
	DiffTime -> EvReqs (Singleton TryWait) -> m (EvOccs (Singleton TryWait), AB)
handleB t' reqs = bool
	(pure (singleton $ OccTryWait t', A))
	(pure (singleton $ OccTryWait t, B $ t' - t))
	(t < t')
	where TryWaitReq t = extract reqs
	
runSleep' :: IO ()
runSleep' = interpretSt A handle' print tickTack
