{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Event.ThreadId (
	-- * GetThreadId
	GetThreadId, pattern OccGetThreadId, getThreadId,
	-- * ThreadId
	ThreadId ) where

import Control.Moffy.Internal.React.Type (React, Request(..), ThreadId, await')
import Data.Type.Set (numbered, Singleton)

---------------------------------------------------------------------------

data GetThreadId = GetThreadIdReq deriving (Show, Eq, Ord)
numbered [t| GetThreadId |]
instance Request GetThreadId where data Occurred GetThreadId = OccGetThreadId

getThreadId :: React s (Singleton GetThreadId) ThreadId
getThreadId = await' GetThreadIdReq \t OccGetThreadId -> t
