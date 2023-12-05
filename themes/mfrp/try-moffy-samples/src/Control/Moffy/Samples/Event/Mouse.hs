{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Samples.Event.Mouse where

import Prelude hiding (repeat)

import Control.Moffy
import Data.Type.Set

data Button = ButtonPrimary | ButtonMiddle | ButtonSecondary deriving (Show, Eq)

data Down = DownReq deriving (Show, Eq, Ord)
numbered [t| Down |]
instance Request Down where data Occurred Down = OccDown Button deriving Show

down :: React s (Singleton Down) Button
down = await DownReq \(OccDown b) -> b

data Up = UpReq deriving (Show, Eq, Ord)
numbered [t| Up |]
instance Request Up where data Occurred Up = OccUp Button deriving Show

up :: React s (Singleton Up) Button
up = await UpReq \(OccUp b) -> b

data Move = MoveReq deriving (Show, Eq, Ord)
numbered [t| Move |]
instance Request Move where data Occurred Move = OccMove Point deriving Show
type Point = (Double, Double)

move :: React s (Singleton Move) Point
move = await MoveReq \(OccMove p) -> p

position :: Sig s (Singleton Move) Point r
position = repeat move

type Events = Move :- Down :- Up :- 'Nil
