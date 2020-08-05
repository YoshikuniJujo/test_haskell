{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Boxes.Event.Time (
	-- * GENERAL
	pattern OccDeltaTime, pattern OccTryWait,
	-- * TIME
	TimeEv, TryWait(..), sleep, DeltaTime(..), deltaTime ) where

import Control.Moffy
import Data.Type.Set (Set(Nil), Singleton, (:-), numbered)
import Data.Bool (bool)
import Data.Time (DiffTime)

---------------------------------------------------------------------------

newtype TryWait = TryWaitReq { getTryWaitReq :: DiffTime } deriving (Show, Eq, Ord)
numbered 64 [t| TryWait |]
instance Request TryWait where
	data Occurred TryWait = OccTryWait DiffTime deriving (Show, Eq, Ord)

tryWait :: DiffTime -> React s (Singleton TryWait) DiffTime
tryWait t = await (TryWaitReq t) \(OccTryWait t') -> t'

sleep :: DiffTime -> React s (Singleton TryWait) ()
sleep t = tryWait t >>= \t' -> bool (sleep (t - t')) (pure ()) (t' == t)

data DeltaTime = DeltaTimeReq deriving (Show, Eq, Ord)
numbered 64 [t| DeltaTime |]
instance Request DeltaTime where
	data Occurred DeltaTime = OccDeltaTime DiffTime deriving (Show, Eq, Ord)

deltaTime :: React s (Singleton DeltaTime) DiffTime
deltaTime = await DeltaTimeReq \(OccDeltaTime t) -> t

type TimeEv = TryWait :- DeltaTime :- 'Nil
