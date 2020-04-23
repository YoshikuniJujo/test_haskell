{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Followbox.ThreadId where

import Data.Type.Set
import Data.Or

import MonadicFrp
import MonadicFrp.ThreadId

data GetThreadId = GetThreadIdReq deriving (Show, Eq, Ord)
numbered 8 [t| GetThreadId |]
instance Request GetThreadId where data Occurred GetThreadId = OccGetThreadId

getThreadId :: React (Singleton GetThreadId) ThreadId
getThreadId = await' GetThreadIdReq \ti OccGetThreadId -> ti

sample1 :: React (Singleton GetThreadId) (ThreadId `Or` ThreadId `Or` ThreadId `Or` ThreadId)
sample1 = getThreadId `first` getThreadId `first` getThreadId `first` getThreadId
