{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Event.ThreadId (
	GetThreadId, Occurred(OccGetThreadId), ThreadId,
	Control.Moffy.Event.ThreadId.getThreadId ) where

import Data.Type.Set

import Control.Moffy.Internal.React.Type

data GetThreadId = GetThreadIdReq deriving (Show, Eq, Ord)
numbered 9 [t| GetThreadId |]
instance Request GetThreadId where data Occurred GetThreadId = OccGetThreadId

getThreadId :: React s (Singleton GetThreadId) ThreadId
getThreadId = await' GetThreadIdReq \ti OccGetThreadId -> ti
