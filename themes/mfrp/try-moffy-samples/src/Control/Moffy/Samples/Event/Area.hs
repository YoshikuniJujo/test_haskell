{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Samples.Event.Area where

import Control.Moffy
import Data.Type.Set
import Data.Bool

type Point = (Double, Double)

data SetArea = SetAreaReq Int Point Point deriving (Show, Eq, Ord)
numbered [t| SetArea |]
instance Request SetArea where data Occurred SetArea = OccSetArea deriving Show

setArea :: Int -> (Point, Point) -> React s (Singleton SetArea) ()
setArea i (lu, rd) = await (SetAreaReq i lu rd) $ const ()

data GetArea = GetAreaReq Int deriving (Show, Eq, Ord)
numbered [t| GetArea |]

instance Request GetArea where
	data Occurred GetArea = OccGetArea Int Point Point deriving Show

getArea :: Int -> React s (Singleton GetArea) (Point, Point)
getArea i0 = go >>= \(i, a) -> bool (getArea i0) (pure a) (i == i0)
	where
	go = await (GetAreaReq i0) \(OccGetArea i lu rd) -> (i, (lu, rd))
