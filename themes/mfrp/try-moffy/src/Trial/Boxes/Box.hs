{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Boxes.Box (
	-- * Box
	Box(..), Rect(..), Color(..) ) where

import Control.Moffy.Event.Mouse (Point)

data Box = Box Rect Color deriving Show
data Rect = Rect { leftup :: Point, rightdown :: Point  } deriving Show
data Color = Red | Green | Blue | Yellow | Cyan | Magenta deriving (Show, Enum)
