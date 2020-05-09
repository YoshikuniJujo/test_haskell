{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Boxes.View (Box(..), Rect(..), Color(..)) where

data Box = Box Rect Color deriving Show
data Rect = Rect { leftup :: Point, rightdown :: Point } deriving Show
data Color = Red | Green | Blue | Yellow | Cyan | Magenta deriving (Show, Enum)

type Point = (Integer, Integer)
