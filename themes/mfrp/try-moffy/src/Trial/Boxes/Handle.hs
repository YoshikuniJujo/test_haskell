{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Boxes.Handle (Mode(InitMode), handleBoxes') where

import Control.Monad.State
import Control.Moffy.Handle hiding (expand)
import Data.Time (DiffTime)
import Data.Time.Clock.TAI (AbsoluteTime)
	
import Control.Moffy.Handle.XField
import Trial.Boxes.Event (BoxEv)
import Field (Field)

import Trial.Boxes.Handle.TimeEv

---------------------------------------------------------------------------

handleBoxes' :: DiffTime -> Field -> HandleSt (Mode, AbsoluteTime) IO BoxEv
handleBoxes' dt f rqs (md, tai) = do
	((os, md'), tai') <- retrySt (handleTimeEvPlus (handle . Just) dt f) rqs md `runStateT` tai
	pure (os, (md', tai'))
