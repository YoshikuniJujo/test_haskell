{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Moffy.EventHandle.ThreadId (
	ThreadId, GetThreadId, Moffy.EventHandle.ThreadId.getThreadId,
	handleGetThreadId) where

import Data.Type.Set
import Data.OneOrMore

import Control.Moffy.Internal.React.Common

data GetThreadId = GetThreadIdReq deriving (Show, Eq, Ord)
numbered 9 [t| GetThreadId |]
instance Request GetThreadId where data Occurred GetThreadId = OccGetThreadId

handleGetThreadId :: Applicative m => Handle' m (Singleton GetThreadId)
handleGetThreadId _rqs = pure . Just $ singleton OccGetThreadId

getThreadId :: React s (Singleton GetThreadId) ThreadId
getThreadId = await' GetThreadIdReq \ti OccGetThreadId -> ti
