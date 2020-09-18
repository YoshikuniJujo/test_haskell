{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Draw.Event where

import Control.Moffy
import Control.Moffy.Viewable.Basic
import Data.Type.Set

newtype StorePoints = StorePointsReq [Position] deriving (Show, Eq, Ord)
numbered [t| StorePoints |]
instance Request StorePoints where data Occurred StorePoints = OccStorePoints

data LoadPoints = LoadPointsReq deriving (Show, Eq, Ord)
numbered [t| LoadPoints |]
instance Request LoadPoints where data Occurred LoadPoints = OccLoadPoints [Position]
