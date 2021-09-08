{-# LANGUAGE DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Paper where

import Control.Moffy
import Control.Moffy.Event.DefaultWindow
import Control.Moffy.Event.Mouse.DefaultWindow
import Data.Type.Flip
import Data.Type.Set

sameClick :: React s (LoadDefaultWindow :- MouseDown :- 'Nil) Bool
sameClick = (==) <$> mouseDown <*> mouseDown

curRect :: Point -> Sig s (LoadDefaultWindow :- MouseMove :- 'Nil) Rect r
curRect p1 = Rect p1 <$%> mousePos

data Rect = Rect { leftup :: Point, rightdown :: Point } deriving Show
