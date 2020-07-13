{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Handle.ThreadId (GetThreadId, handleGetThreadId) where

import Data.Type.Set
import Data.OneOrMore

import Control.Moffy.Handle
import Control.Moffy.Event.ThreadId

handleGetThreadId :: Applicative m => Handle' m (Singleton GetThreadId)
handleGetThreadId _rqs = pure . Just $ singleton OccGetThreadId
