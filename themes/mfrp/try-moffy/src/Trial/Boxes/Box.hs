module Trial.Boxes.Box where

import Control.Moffy.Event.Mouse

data Color = Red | Green | Blue | Yellow | Cyan | Magenta deriving (Show, Enum)
data Rect = Rect { leftup :: Point, rightdown :: Point  } deriving Show
data Box = Box Rect Color deriving Show
