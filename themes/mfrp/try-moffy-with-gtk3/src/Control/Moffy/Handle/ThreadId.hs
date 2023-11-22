{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Handle.ThreadId (
	-- * GetThreadId
	GetThreadId,
	-- * Handle
	handleGetThreadId ) where

import Control.Moffy.Event.ThreadId (GetThreadId, pattern OccGetThreadId)
import Control.Moffy.Handle (Handle')
import Data.Type.Set (Singleton)

import Data.OneOrMoreApp

---------------------------------------------------------------------------

handleGetThreadId :: Applicative m => Handle' m (Singleton GetThreadId)
handleGetThreadId _rqs = pure . Just $ Singleton OccGetThreadId
