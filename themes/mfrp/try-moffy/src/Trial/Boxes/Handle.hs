{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Boxes.Handle (Mode(InitMode), handleBoxes) where

import Control.Moffy.Handle hiding (expand)
import Control.Monad.State (StateT)
import Data.Time (DiffTime)
import Data.Time.Clock.TAI (AbsoluteTime)
	
import Control.Moffy.Handle.XField
import Trial.Boxes.Event (BoxEv)
import Field (Field)

import Trial.Boxes.Handle.TimeEv

---------------------------------------------------------------------------

handleBoxes :: DiffTime -> Field -> HandleSt Mode (StateT AbsoluteTime IO) BoxEv
handleBoxes = (retrySt .) . handleTimeEvPlus (handle . Just)
