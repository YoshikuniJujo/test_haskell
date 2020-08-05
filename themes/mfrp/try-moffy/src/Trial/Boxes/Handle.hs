{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Boxes.Handle (Mode(InitMode), handleBoxes) where

import Control.Moffy.Handle hiding (expand)
import Data.Time (DiffTime)
import Data.Time.Clock.TAI (AbsoluteTime)
	
import Control.Moffy.Handle.XField
import Trial.Boxes.Event (BoxEv)
import Field (Field)

import Trial.Boxes.Handle.TimeEv

---------------------------------------------------------------------------

handleBoxes :: DiffTime -> Field -> HandleSt (Mode, AbsoluteTime) IO BoxEv
handleBoxes dt f = retrySt $ fromSt (handleTimeEvPlus handle') dt f

handle' :: HandleSt' (DiffTime, Field, ()) () IO GuiEv
handle' rqs (dt, f, ()) = (, ()) <$> handle (Just dt) f rqs

fromSt :: HandleSt' (a, b, s) s' m es -> a -> b -> HandleSt' s s' m es
fromSt hdl x y rqs s = hdl rqs (x, y, s)
