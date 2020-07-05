{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Moffy.EventHandle.ThreadId.Handle (GetThreadId, handleGetThreadId) where

import Data.Type.Set
import Data.OneOrMore

import Control.Moffy.Internal.React.Type
import Moffy.EventHandle.ThreadId.Event

handleGetThreadId :: Applicative m => Handle' m (Singleton GetThreadId)
handleGetThreadId _rqs = pure . Just $ singleton OccGetThreadId
