{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Draw.Event where

import Control.Moffy
import Control.Moffy.Viewable.Basic
import Data.Type.Set

import qualified Data.Set as D

type SimpleLine = (Position, Position)

newtype StoreLines = StoreLinesReq (D.Set SimpleLine) deriving (Show, Eq, Ord)
numbered [t| StoreLines |]
instance Request StoreLines where data Occurred StoreLines = OccStoreLines

data LoadLines = LoadLinesReq deriving (Show, Eq, Ord)
numbered [t| LoadLines |]
instance Request LoadLines where data Occurred LoadLines = OccLoadLines (D.Set SimpleLine)
