{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Viewable.Shape (
	Line(..), Box(..), Rect(..), BColor(..), FillPolygon(..) ) where

import Data.Type.Set

import Control.Moffy.Viewable.Basic

data Line = Line' Color LineWidth Position Position deriving Show
numbered [t| Line |]

data Box = Box Rect BColor deriving Show
data Rect = Rect { leftup :: Position, rightdown :: Position  } deriving Show
data BColor = Red | Green | Blue | Yellow | Cyan | Magenta deriving (Show, Enum)
numbered [t| Box |]

data FillPolygon = FillPolygon Color [Position] deriving Show
numbered [t| FillPolygon |]
