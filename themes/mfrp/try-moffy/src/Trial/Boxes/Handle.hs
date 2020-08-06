{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Boxes.Handle (Mode(InitMode), handleBoxes) where

import Control.Moffy.Handle hiding (expand)
import Control.Moffy.Handle.Time
import Data.Time (DiffTime)
import Data.Time.Clock.TAI (AbsoluteTime)
	
import Control.Moffy.Handle.XField
import Trial.Boxes.Event (BoxEv)
import Field (Field)

---------------------------------------------------------------------------

handleBoxes :: DiffTime -> Field -> HandleSt (Mode, AbsoluteTime) IO BoxEv
handleBoxes = ((retrySt .) .) . curry . popInput . handleTimeEvPlus
	. pushInput . uncurry $ (liftHandle' .) . handle . Just
