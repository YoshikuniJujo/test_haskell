{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}
{-# LANGUAGE FlexibleInstances #-}

module Trial.Draw.Handle where

import Control.Moffy.Handle
import Data.Type.Set

import qualified Data.OneOrMore as Oom
import qualified Data.OneOrMoreApp as Ooma
import qualified Data.Set as D

import Trial.Draw.Event

class LinesState s where
	getLines :: s -> D.Set SimpleLine
	putLines :: s -> D.Set SimpleLine -> s

instance LinesState (D.Set SimpleLine) where
	getLines = id
	putLines = flip const

handleLines :: (LinesState s, Monad m) => HandleSt' s m LinesEv
handleLines = handleStoreLines `mergeSt` handleLoadLines

handleStoreLines :: (LinesState s, Applicative m) =>
	HandleSt' s m (Singleton StoreLines)
handleStoreLines (Oom.Singleton (StoreLinesReq ls)) s =
	pure (Just $ Ooma.Singleton OccStoreLines, s `putLines` ls)

handleLoadLines :: (LinesState s, Applicative m) =>
	HandleSt' s m (Singleton LoadLines)
handleLoadLines _rqs s =
	pure (Just . Ooma.Singleton . OccLoadLines $ getLines s, s)
