{-# LANGUAGE DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Paper where

import Control.Moffy
import Control.Moffy.Event.DefaultWindow
import Control.Moffy.Event.Mouse.DefaultWindow
import Data.Type.Set

sameClick :: React s (LoadDefaultWindow :- MouseDown :- 'Nil) Bool
sameClick = (==) <$> mouseDown <*> mouseDown
