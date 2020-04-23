{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Followbox.ThreadId (
	GetThreadId, Occurred(OccGetThreadId), handleGetThreadId, getThreadId
	) where

import Data.Type.Set
import Data.UnionSet

import MonadicFrp
import MonadicFrp.Handle
import MonadicFrp.ThreadId

data GetThreadId = GetThreadIdReq deriving (Show, Eq, Ord)
numbered 8 [t| GetThreadId |]
instance Request GetThreadId where data Occurred GetThreadId = OccGetThreadId

handleGetThreadId :: Applicative m => Handle' m (Singleton GetThreadId)
handleGetThreadId _reqs = pure . Just $ singleton OccGetThreadId

getThreadId :: React (Singleton GetThreadId) ThreadId
getThreadId = await' GetThreadIdReq \ti OccGetThreadId -> ti
