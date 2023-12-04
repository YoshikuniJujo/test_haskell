{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Samples.Boxes.Viewable where

data Box = Box Rect BColor deriving Show
data BColor = Red | Green | Blue | Yellow | Cyan | Magenta deriving (Show, Enum)

-- data Rect = Rect { leftUp :: Point, rightdown :: Point }
data Rect = Rect Point Point deriving Show

type Point = (Double, Double)
