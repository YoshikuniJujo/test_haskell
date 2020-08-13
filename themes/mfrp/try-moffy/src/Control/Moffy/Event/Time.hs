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

import Control.Moffy (React, Request(..), await)
import Data.Type.Set (numbered, pattern Nil, Singleton, (:-))
import Data.Bool (bool)
import Data.Time (DiffTime)

---------------------------------------------------------------------------

-- * SLEEP
-- * DELTA TIME
-- * TIME EV

---------------------------------------------------------------------------


---------------------------------------------------------------------------
-- SLEEP
---------------------------------------------------------------------------

newtype TryWait = TryWaitReq DiffTime deriving (Show, Eq, Ord)
numbered [t| TryWait |]
instance Request TryWait where
	data Occurred TryWait = OccTryWait DiffTime deriving (Show, Eq, Ord)

tryWait :: DiffTime -> React s (Singleton TryWait) DiffTime
tryWait t = await (TryWaitReq t) \(OccTryWait t') -> t'

sleep :: DiffTime -> React s (Singleton TryWait) ()
sleep t = tryWait t >>= \t' -> bool (sleep (t - t')) (pure ()) (t' == t)

---------------------------------------------------------------------------
-- DELTA TIME
---------------------------------------------------------------------------

data DeltaTime = DeltaTimeReq deriving (Show, Eq, Ord)
numbered [t| DeltaTime |]
instance Request DeltaTime where
	data Occurred DeltaTime = OccDeltaTime DiffTime deriving (Show, Eq, Ord)

deltaTime :: React s (Singleton DeltaTime) DiffTime
deltaTime = await DeltaTimeReq \(OccDeltaTime t) -> t

---------------------------------------------------------------------------
-- TIME EV
---------------------------------------------------------------------------

type TimeEv = DeltaTime :- TryWait :- 'Nil
