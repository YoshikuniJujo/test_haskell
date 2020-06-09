{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MonadicFrp.EventHandle.ThreadId (
	-- * Types
	ThreadId, GetThreadId, Occurred(..),
	-- * React
	getThreadId,
	-- * Handle
	handleGetThreadId
	) where

import Data.Type.Set (Singleton, numbered)
import Data.UnionSet (singleton)

import MonadicFrp (React, Request(..), await')
import MonadicFrp.Handle (Handle')
import MonadicFrp.ThreadId.Type (ThreadId)

---------------------------------------------------------------------------

data GetThreadId = GetThreadIdReq deriving (Show, Eq, Ord)
numbered 9 [t| GetThreadId |]
instance Request GetThreadId where data Occurred GetThreadId = OccGetThreadId

handleGetThreadId :: Applicative m => Handle' m (Singleton GetThreadId)
handleGetThreadId _reqs = pure . Just $ singleton OccGetThreadId

getThreadId :: React (Singleton GetThreadId) ThreadId
getThreadId = await' GetThreadIdReq \ti OccGetThreadId -> ti
