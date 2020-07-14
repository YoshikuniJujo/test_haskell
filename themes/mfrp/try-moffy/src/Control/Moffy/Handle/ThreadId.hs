{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Handle.ThreadId (GetThreadId, handleGetThreadId) where

import Control.Moffy.Handle (Handle')
import Control.Moffy.Event.ThreadId (GetThreadId, pattern OccGetThreadId)
import Data.Type.Set (Singleton)
import Data.OneOrMore (singleton)

handleGetThreadId :: Applicative m => Handle' m (Singleton GetThreadId)
handleGetThreadId _rqs = pure . Just $ singleton OccGetThreadId
