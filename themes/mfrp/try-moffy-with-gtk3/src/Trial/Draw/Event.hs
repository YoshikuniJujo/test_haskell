{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Draw.Event where

import Control.Moffy
import Control.Moffy.Event.ThreadId
import Control.Moffy.Event.Lock
import Control.Moffy.Viewable.Basic
import Data.Type.Set

import qualified Data.Set as D

type SimpleLine = (Position, Position)

newtype StoreLines = StoreLinesReq (D.Set SimpleLine) deriving (Show, Eq, Ord)
numbered [t| StoreLines |]
instance Request StoreLines where data Occurred StoreLines = OccStoreLines

storeLines :: D.Set SimpleLine -> React s (Singleton StoreLines) ()
storeLines ln = await (StoreLinesReq ln) $ const ()

data LoadLines = LoadLinesReq deriving (Show, Eq, Ord)
numbered [t| LoadLines |]
instance Request LoadLines where data Occurred LoadLines = OccLoadLines (D.Set SimpleLine)

loadLines :: React s (Singleton LoadLines) (D.Set SimpleLine)
loadLines = await LoadLinesReq $ \(OccLoadLines ls) -> ls

addLine :: LockId -> SimpleLine -> React s (StoreLines :- LoadLines :- GetThreadId :- LockEv) ()
addLine li ln = withLock li do
	ls <- adjust loadLines
	adjust . storeLines $ ln `D.insert` ls :: React s (StoreLines :- LoadLines :- 'Nil) ()

type LinesEv = StoreLines :- LoadLines :- 'Nil
