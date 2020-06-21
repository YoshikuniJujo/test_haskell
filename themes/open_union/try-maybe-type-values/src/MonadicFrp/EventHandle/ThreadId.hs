{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MonadicFrp.EventHandle.ThreadId (
	-- * Type
	ThreadId, GetThreadId, Occurred,
	-- * Handle
	handleGetThreadId,
	-- * Event
	getThreadId ) where

import Data.Type.Set (Singleton, numbered)
import Data.UnionSet (singleton)

import MonadicFrp (React, Request(..))
import MonadicFrp.React (ThreadId, await')
import MonadicFrp.Handle (Handle')

---------------------------------------------------------------------------

data GetThreadId = GetThreadIdReq deriving (Show, Eq, Ord)
numbered 9 [t| GetThreadId |]
instance Request GetThreadId where data Occurred GetThreadId = OccGetThreadId

handleGetThreadId :: Applicative m => Handle' m (Singleton GetThreadId)
handleGetThreadId _rqs = pure . Just $ singleton OccGetThreadId

getThreadId :: React (Singleton GetThreadId) ThreadId
getThreadId = await' GetThreadIdReq \ti OccGetThreadId -> ti
