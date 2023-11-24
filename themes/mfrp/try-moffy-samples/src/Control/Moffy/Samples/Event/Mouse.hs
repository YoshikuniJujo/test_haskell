{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Samples.Event.Mouse where

import Control.Moffy
import Data.Type.Set

data Button = ButtonPrimary | ButtonMiddle | ButtonSecondary deriving (Show, Eq)

data Down = DownReq deriving (Show, Eq, Ord)
numbered [t| Down |]
instance Request Down where data Occurred Down = OccDown Button deriving Show

down :: React s (Singleton Down) Button
down = await DownReq \(OccDown b) -> b
