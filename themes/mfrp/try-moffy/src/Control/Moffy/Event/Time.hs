{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Event.Time (
	-- * Time Ev
	TimeEv,
	-- * Delta Time
	DeltaTime(..), pattern OccDeltaTime, deltaTime,
	-- * Sleep
	TryWait(..), pattern OccTryWait, sleep ) where

import GHC.Stack (HasCallStack)
import Control.Moffy (React, Request(..), await)
import Data.Type.Set (numbered, pattern Nil, Singleton, (:-))
import Data.Bool (bool)
import Data.Time (DiffTime)

---------------------------------------------------------------------------

-- * DELTA TIME
-- * SLEEP
-- * TIME EV

---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- DELTA TIME
---------------------------------------------------------------------------

data DeltaTime = DeltaTimeReq deriving (Show, Eq, Ord)
numbered [t| DeltaTime |]
instance Request DeltaTime where data Occurred DeltaTime = OccDeltaTime DiffTime

deltaTime :: React s (Singleton DeltaTime) DiffTime
deltaTime = await DeltaTimeReq \(OccDeltaTime t) -> t

---------------------------------------------------------------------------
-- SLEEP
---------------------------------------------------------------------------

newtype TryWait = TryWaitReq DiffTime deriving (Show, Eq, Ord)
numbered [t| TryWait |]
instance Request TryWait where data Occurred TryWait = OccTryWait DiffTime

tryWait :: DiffTime -> React s (Singleton TryWait) DiffTime
tryWait t = await (TryWaitReq t) \(OccTryWait t') -> t'

sleep :: HasCallStack => DiffTime -> React s (Singleton TryWait) ()
sleep t | t <= 0 = error "sleep t: t (seconds) should be positive"
sleep t = tryWait t >>= \t' -> bool (sleep (t - t')) (pure ()) (t' == t)

---------------------------------------------------------------------------
-- TIME EV
---------------------------------------------------------------------------

type TimeEv = DeltaTime :- TryWait :- 'Nil
