{-# LANGUAGE DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Paper where

import Control.Moffy
import Control.Moffy.Event.DefaultWindow
import Control.Moffy.Event.Mouse.DefaultWindow
import Control.Moffy.Viewable.Shape
import Data.Type.Flip
import Data.Type.Set

sameClick :: React s (LoadDefaultWindow :- MouseDown :- 'Nil) Bool
sameClick = (==) <$> mouseDown <*> mouseDown

curRect :: Point -> Sig s (LoadDefaultWindow :- MouseMove :- 'Nil) Rect r
curRect p1 = Rect p1 <$%> mousePos

posInside :: Rect -> Sig s es (Double, Double) () -> React s es (Either (Double, Double) ())
posInside rct = find (`inside` rct)
	where (x, y) `inside` Rect (l, u) (r, d) =
		(l <= x && x <= r || r <= x && x <= l) &&
		(u <= y && y <= d || d <= y && y <= u)
