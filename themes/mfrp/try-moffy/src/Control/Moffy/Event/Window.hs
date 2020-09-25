{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Event.Window (
	WindowId(..), WindowNew(..), pattern OccWindowNew, windowNew ) where

import Control.Moffy
import Data.Type.Set

newtype WindowId = WindowId Integer deriving Show

data WindowNew = WindowNewReq deriving (Show, Eq, Ord)
numbered [t| WindowNew |]
instance Request WindowNew where
	data Occurred WindowNew = OccWindowNew WindowId deriving Show

windowNew :: React s (Singleton WindowNew) WindowId
windowNew = await WindowNewReq \(OccWindowNew wid) -> wid
